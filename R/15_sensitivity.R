# 15_sensitivity.R
# Sensitivity analyses: EV threshold sweep, odds-source sweep, per-season
# stability, and quarter-Kelly stake sizing robustness check.

# -----------------------------------------------------------------------------
# From 03_sensitivity.R
# -----------------------------------------------------------------------------

# Section 5.4 - Sensitivity analyses around Strategy A (Regime 1 only):
#   1. EV threshold tau in {0, 0.02, 0.05, 0.10}
#   2. Odds source: PSC (default) vs Max-across-6 vs Mean-across-6
#   3. Time stability: per-season ROI

set.seed(441)
suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
})

root     <- "~/Desktop/FLB_Final_Code"
data_dir <- file.path(root, "data")
fig_dir  <- file.path(root, "figures")
tbl_dir  <- file.path(root, "tables")

theme_eda <- theme_bw() + theme(panel.grid.minor = element_blank())
save_png <- function(p, name, w = 8, h = 5) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p,
         width = w, height = h, dpi = 300)
}

s4_data <- "~/Desktop/FLB_Final_Code/data"
test     <- readRDS(file.path(s4_data, "test_features.rds"))
baseline <- readRDS(file.path(s4_data, "baseline_preds_test.rds"))
en       <- readRDS(file.path(s4_data, "elasticnet_preds_test.rds"))
xgb      <- readRDS(file.path(s4_data, "xgb_preds_test.rds"))

# Build (match_id, regime, FTR, probs, odds-per-book) table for each model
join_preds <- function(preds, p_prefix) {
  test %>%
    select(match_id, Date, Regime, FTR, Season,
           B365H, B365D, B365A,
           BWH, BWD, BWA,
           IWH, IWD, IWA,
           WHH, WHD, WHA,
           VCH, VCD, VCA,
           PSCH, PSCD, PSCA) %>%
    left_join(preds %>% select(match_id,
                               !!sym(paste0(p_prefix, "H")),
                               !!sym(paste0(p_prefix, "D")),
                               !!sym(paste0(p_prefix, "A"))),
              by = "match_id") %>%
    rename(pH = !!sym(paste0(p_prefix, "H")),
           pD = !!sym(paste0(p_prefix, "D")),
           pA = !!sym(paste0(p_prefix, "A")))
}

df_shin <- join_preds(baseline, "p_shin_")
df_en   <- join_preds(en,       "p_en_")
df_xgb  <- join_preds(xgb,      "p_xgb_")

# Odds aggregates per outcome: max / mean / Pinnacle close
add_odds_aggregates <- function(df) {
  df %>%
    mutate(
      maxH  = pmax(B365H, BWH, IWH, WHH, VCH, PSCH),
      maxD  = pmax(B365D, BWD, IWD, WHD, VCD, PSCD),
      maxA  = pmax(B365A, BWA, IWA, WHA, VCA, PSCA),
      meanH = (B365H + BWH + IWH + WHH + VCH + PSCH) / 6,
      meanD = (B365D + BWD + IWD + WHD + VCD + PSCD) / 6,
      meanA = (B365A + BWA + IWA + WHA + VCA + PSCA) / 6
    )
}

df_shin <- add_odds_aggregates(df_shin)
df_en   <- add_odds_aggregates(df_en)
df_xgb  <- add_odds_aggregates(df_xgb)

# -- Core strategy simulator given an odds source + threshold + regime set
run_strategy <- function(df, odds_prefix, tau = 0, regimes = "1") {
  oH <- df[[paste0(odds_prefix, "H")]]
  oD <- df[[paste0(odds_prefix, "D")]]
  oA <- df[[paste0(odds_prefix, "A")]]

  EV <- cbind(df$pH * oH - 1,
              df$pD * oD - 1,
              df$pA * oA - 1)
  best_idx <- max.col(EV, ties.method = "first")
  best_EV  <- EV[cbind(seq_len(nrow(EV)), best_idx)]

  outcomes <- c("H","D","A")
  bet_outcome <- outcomes[best_idx]
  bet_odds <- ifelse(bet_outcome == "H", oH,
              ifelse(bet_outcome == "D", oD, oA))

  df2 <- df %>%
    mutate(best_EV = best_EV,
           bet_outcome = bet_outcome,
           bet_odds = bet_odds) %>%
    filter(best_EV > tau,
           as.character(Regime) %in% regimes)

  if (nrow(df2) == 0) return(tibble(N = 0L, ROI = NA_real_, Total_profit = 0))

  won    <- df2$bet_outcome == as.character(df2$FTR)
  profit <- ifelse(won, df2$bet_odds - 1, -1)
  tibble(N = nrow(df2),
         Total_profit = sum(profit),
         ROI = sum(profit) / nrow(df2))
}

models <- list(
  list(name = "Shin (1993)", df = df_shin),
  list(name = "Elastic Net", df = df_en),
  list(name = "XGBoost",     df = df_xgb)
)

# ---------- Check 1: EV threshold tau ------------------------------------
taus <- c(0, 0.02, 0.05, 0.10)
res_tau <- expand_grid(Model = sapply(models, function(m) m$name),
                       tau = taus) %>%
  rowwise() %>%
  mutate(out = list({
    mdf <- models[[which(sapply(models, function(m) m$name) == Model)]]$df
    run_strategy(mdf, "PSC", tau = tau, regimes = "1")
  })) %>%
  ungroup() %>%
  unnest_wider(out)

write_csv(res_tau %>% mutate(across(where(is.numeric), ~ signif(., 4))),
          file.path(tbl_dir, "table_5_3_sensitivity_threshold.csv"))
tex <- kable(res_tau %>% mutate(across(where(is.numeric), ~ signif(., 4))),
             format = "latex", booktabs = TRUE,
             caption = "Strategy A sensitivity: ROI at varying EV thresholds tau (Regime 1 only, Pinnacle closing).",
             label = "tab:sensitivity-threshold") %>%
  kable_styling(latex_options = c("hold_position"))
writeLines(as.character(tex), file.path(tbl_dir, "table_5_3_sensitivity_threshold.tex"))

# ---------- Check 2: Odds source -----------------------------------------
odds_sources <- list(
  list(name = "Pinnacle closing", prefix = "PSC"),
  list(name = "Max of 6 books",   prefix = "max"),
  list(name = "Mean of 6 books",  prefix = "mean")
)

res_odds <- expand_grid(Model = sapply(models, function(m) m$name),
                        Odds  = sapply(odds_sources, function(o) o$name)) %>%
  rowwise() %>%
  mutate(out = list({
    mdf   <- models[[which(sapply(models, function(m) m$name) == Model)]]$df
    opref <- odds_sources[[which(sapply(odds_sources, function(o) o$name) == Odds)]]$prefix
    run_strategy(mdf, opref, tau = 0, regimes = "1")
  })) %>%
  ungroup() %>%
  unnest_wider(out)

write_csv(res_odds %>% mutate(across(where(is.numeric), ~ signif(., 4))),
          file.path(tbl_dir, "table_5_4_sensitivity_odds.csv"))
tex <- kable(res_odds %>% mutate(across(where(is.numeric), ~ signif(., 4))),
             format = "latex", booktabs = TRUE,
             caption = "Strategy A sensitivity: ROI by odds source (Regime 1 only, EV > 0).",
             label = "tab:sensitivity-odds") %>%
  kable_styling(latex_options = c("hold_position"))
writeLines(as.character(tex), file.path(tbl_dir, "table_5_4_sensitivity_odds.tex"))

# ---------- Check 3: Time stability --------------------------------------
bets_files <- c("bets_shin.rds","bets_en.rds","bets_xgb.rds")
model_names <- c("Shin (1993)","Elastic Net","XGBoost")

res_season <- map2_dfr(bets_files, model_names, function(f, n) {
  df <- readRDS(file.path(data_dir, f)) %>% filter(bet_placed)
  df %>%
    left_join(test %>% select(match_id, Season), by = "match_id") %>%
    filter(as.character(Regime) == "1") %>%
    group_by(Season) %>%
    summarise(Model = n,
              N = n(),
              Total_profit = sum(profit),
              ROI = sum(profit) / n(),
              .groups = "drop") %>%
    relocate(Model)
})

write_csv(res_season %>% mutate(across(where(is.numeric), ~ signif(., 4))),
          file.path(tbl_dir, "table_5_5_sensitivity_by_season.csv"))
tex <- kable(res_season %>% mutate(across(where(is.numeric), ~ signif(., 4))),
             format = "latex", booktabs = TRUE,
             caption = "Strategy A (Regime 1) ROI by season (Pinnacle closing, EV > 0).",
             label = "tab:sensitivity-season") %>%
  kable_styling(latex_options = c("hold_position"))
writeLines(as.character(tex), file.path(tbl_dir, "table_5_5_sensitivity_by_season.tex"))

# ---------- Figure 5.2: ROI by threshold --------------------------------
p_thr <- ggplot(res_tau, aes(x = tau, y = ROI, colour = Model)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_colour_brewer(palette = "Set1") +
  scale_x_continuous(breaks = taus) +
  labs(x = "EV threshold tau",
       y = "ROI",
       title = "Figure 5.2 - ROI vs EV threshold (Strategy A: Regime 1 only)",
       subtitle = "Pinnacle closing; flat 1-unit stakes; dashed line at ROI = 0") +
  theme_eda
save_png(p_thr, "figure_5_2_roi_by_threshold", w = 8, h = 5)

# ---------- Console findings --------------------------------------------
cat("==== Section 5.4 findings ====\n")
cat("1. Threshold sensitivity (Strategy A, Pinnacle closing):\n")
print(res_tau %>% mutate(across(where(is.numeric), ~ signif(., 4))))

cat("\n2. Odds-source sensitivity (Strategy A, tau = 0):\n")
print(res_odds %>% mutate(across(where(is.numeric), ~ signif(., 4))))

cat("\n3. Time-stability (Strategy A, Pinnacle closing, tau = 0):\n")
print(res_season %>% mutate(across(where(is.numeric), ~ signif(., 4))), n = Inf)

any_positive_tau <- any(res_tau$ROI > 0, na.rm = TRUE)
any_positive_odds <- any(res_odds$ROI > 0, na.rm = TRUE)
any_positive_season <- any(res_season$ROI > 0, na.rm = TRUE)

cat(sprintf("\n4. Any positive ROI across threshold sweep? %s\n",
            ifelse(any_positive_tau, "YES", "NO")))
cat(sprintf("5. Any positive ROI across odds-source sweep? %s  |  any positive season? %s\n",
            ifelse(any_positive_odds,   "YES", "NO"),
            ifelse(any_positive_season, "YES", "NO")))

# -----------------------------------------------------------------------------
# From 05_kelly_sensitivity.R
# -----------------------------------------------------------------------------

# Section 5.5 - Kelly-sizing robustness check.
# Re-sizes the bets ALREADY in the ledgers under 1/4 Kelly with a 0.10 cap.
# Does NOT recompute EVs or re-pick bets.

suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
  library(patchwork)
})

root     <- "~/Desktop/FLB_Final_Code"
data_dir <- file.path(root, "data")
fig_dir  <- file.path(root, "figures")
tbl_dir  <- file.path(root, "tables")

theme_eda <- theme_bw() + theme(panel.grid.minor = element_blank())
save_png <- function(p, name, w = 8, h = 5) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p,
         width = w, height = h, dpi = 300)
}

# -- Load ledgers ---------------------------------------------------------
bets_shin <- readRDS(file.path(data_dir, "bets_shin.rds"))
bets_en   <- readRDS(file.path(data_dir, "bets_en.rds"))
bets_xgb  <- readRDS(file.path(data_dir, "bets_xgb.rds"))

models <- list(
  list(name = "Shin (1993)", df = bets_shin),
  list(name = "Elastic Net", df = bets_en),
  list(name = "XGBoost",     df = bets_xgb)
)

strategies <- list(
  list(name = "Unconditional",       regimes = as.character(1:4)),
  list(name = "A: Regime 1",         regimes = "1"),
  list(name = "B: Regime 1+2",       regimes = c("1","2")),
  list(name = "C: Regime 3+4 (ctl)", regimes = c("3","4"))
)

# -- Kelly sizing ---------------------------------------------------------
kelly_size <- function(df_placed, cap = 0.10, frac = 0.25) {
  f_full  <- (df_placed$p_model * df_placed$bet_odds - 1) /
             (df_placed$bet_odds - 1)
  f_kelly <- frac * f_full
  anomalous <- f_kelly <= 0
  f_kelly <- pmax(f_kelly, 0)
  hit_cap <- f_kelly > cap
  f_kelly <- pmin(f_kelly, cap)
  list(stake = f_kelly, hit_cap = hit_cap, anomalous = anomalous)
}

# -- Per-cell metrics -----------------------------------------------------
eval_kelly <- function(df_placed, model_name, strategy_name, regimes) {
  sub <- df_placed %>% filter(as.character(Regime) %in% regimes)
  if (nrow(sub) == 0) {
    return(tibble(Model = model_name, Strategy = strategy_name,
                  N = 0L, Total_staked = 0, Total_profit = 0,
                  Kelly_ROI = NA_real_, Kelly_Sharpe = NA_real_,
                  Kelly_mean_stake = NA_real_, Kelly_median_stake = NA_real_,
                  Kelly_max_stake = NA_real_, Kelly_cap_hit_pct = NA_real_,
                  Kelly_anomalous_n = 0L))
  }
  ks     <- kelly_size(sub)
  stake  <- ks$stake
  profit <- ifelse(sub$won == 1, stake * (sub$bet_odds - 1), -stake)
  total_staked <- sum(stake)
  total_profit <- sum(profit)
  roi <- if (total_staked > 0) total_profit / total_staked else NA_real_
  # Per-unit-stake return, for a Sharpe comparable to the flat case
  per_unit <- ifelse(sub$won == 1, sub$bet_odds - 1, -1)
  sharpe <- if (sd(per_unit) > 0 && total_staked > 0)
    mean(per_unit) / sd(per_unit) else NA_real_

  tibble(
    Model = model_name, Strategy = strategy_name,
    N = nrow(sub),
    Total_staked = total_staked,
    Total_profit = total_profit,
    Kelly_ROI = roi,
    Kelly_Sharpe = sharpe,
    Kelly_mean_stake   = mean(stake),
    Kelly_median_stake = median(stake),
    Kelly_max_stake    = max(stake),
    Kelly_cap_hit_pct  = mean(ks$hit_cap) * 100,
    Kelly_anomalous_n  = sum(ks$anomalous)
  )
}

kelly_tbl <- map_dfr(models, function(m) {
  placed <- m$df %>% filter(bet_placed)
  map_dfr(strategies, function(s) {
    eval_kelly(placed, m$name, s$name, s$regimes)
  })
})

# -- Merge with flat-stake ROI from table_5_6_headline.csv ----------------
flat <- read_csv(file.path(tbl_dir, "table_5_6_headline.csv"),
                 show_col_types = FALSE) %>%
  select(Strategy, Model, Flat_ROI = ROI, Flat_Sharpe = Sharpe)

merged <- kelly_tbl %>%
  left_join(flat, by = c("Model","Strategy")) %>%
  mutate(Delta_ROI = Kelly_ROI - Flat_ROI) %>%
  relocate(Model, Strategy, N, Flat_ROI, Kelly_ROI, Delta_ROI,
           Kelly_Sharpe, Flat_Sharpe,
           Kelly_mean_stake, Kelly_median_stake, Kelly_max_stake,
           Kelly_cap_hit_pct, Kelly_anomalous_n,
           Total_staked, Total_profit)

# Ordering: strategies in canonical order
merged <- merged %>%
  mutate(Strategy = factor(Strategy,
                           levels = c("Unconditional",
                                      "A: Regime 1",
                                      "B: Regime 1+2",
                                      "C: Regime 3+4 (ctl)"))) %>%
  arrange(Strategy, Model)

out <- merged %>% mutate(across(where(is.numeric), ~ signif(., 4)))
write_csv(out, file.path(tbl_dir, "table_5_7_kelly_vs_flat.csv"))

tex <- kable(out, format = "latex", booktabs = TRUE, longtable = TRUE,
             caption = "Flat vs quarter-Kelly (capped at 0.10) ROI across 12 (model, strategy) cells. Sharpe is per-unit-stake and directly comparable to the flat case.",
             label = "tab:kelly-vs-flat") %>%
  kable_styling(latex_options = c("repeat_header","scale_down"),
                full_width = FALSE)
writeLines(as.character(tex), file.path(tbl_dir, "table_5_7_kelly_vs_flat.tex"))

# -- Figure 5.3: grouped bars, two panels ---------------------------------
long <- merged %>%
  select(Model, Strategy, Flat_ROI, Kelly_ROI) %>%
  pivot_longer(c(Flat_ROI, Kelly_ROI),
               names_to = "Sizing", values_to = "ROI") %>%
  mutate(Sizing = recode(Sizing,
                         Flat_ROI  = "Flat (1 unit)",
                         Kelly_ROI = "Quarter Kelly (cap 0.10)"),
         Sizing = factor(Sizing,
                         levels = c("Flat (1 unit)",
                                    "Quarter Kelly (cap 0.10)")))

ylim <- range(long$ROI, na.rm = TRUE) * 1.1

mk_panel <- function(df, title) {
  ggplot(df, aes(x = Strategy, y = ROI, fill = Model)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_brewer(palette = "Set2") +
    coord_cartesian(ylim = ylim) +
    labs(x = NULL, y = "ROI", title = title) +
    theme_eda +
    theme(axis.text.x = element_text(angle = 10, hjust = 1))
}

p_top <- mk_panel(long %>% filter(Sizing == "Flat (1 unit)"),
                  "Flat 1-unit stakes")
p_bot <- mk_panel(long %>% filter(Sizing == "Quarter Kelly (cap 0.10)"),
                  "Quarter Kelly (cap 0.10)")

p_fig <- (p_top / p_bot) +
  plot_annotation(title = "Figure 5.3 - Flat vs Kelly ROI across 12 cells",
                  subtitle = "Pinnacle closing odds; identical bet selection; only stake size differs")
save_png(p_fig, "figure_5_3_kelly_vs_flat", w = 10, h = 8)

# -- Console findings -----------------------------------------------------
cat("==== Section 5.5 findings ====\n")
cat("1. Kelly vs Flat ROI (signif 4):\n")
print(out %>% select(Strategy, Model, N, Flat_ROI, Kelly_ROI, Delta_ROI,
                     Kelly_cap_hit_pct, Kelly_anomalous_n), n = Inf)

best_flat  <- merged %>% slice_max(Flat_ROI,  n = 1)
best_kelly <- merged %>% slice_max(Kelly_ROI, n = 1)
cat(sprintf("\n2. Best flat cell : %s / %s  (Flat ROI = %+.4f).\n",
            best_flat$Model,  as.character(best_flat$Strategy),  best_flat$Flat_ROI))
cat(sprintf("3. Best Kelly cell: %s / %s  (Kelly ROI = %+.4f).\n",
            best_kelly$Model, as.character(best_kelly$Strategy), best_kelly$Kelly_ROI))

flipped <- merged %>% filter(Flat_ROI < 0 & Kelly_ROI > 0)
if (nrow(flipped) > 0) {
  cat(sprintf("\n4. %d cell(s) flip from negative Flat ROI to POSITIVE Kelly ROI:\n",
              nrow(flipped)))
  print(flipped %>% select(Model, Strategy, Flat_ROI, Kelly_ROI, Delta_ROI) %>%
          mutate(across(where(is.numeric), ~ signif(., 4))))
} else {
  cat("\n4. NO cell flips from negative Flat ROI to positive Kelly ROI.\n")
}

cat(sprintf("\n5. Mean Delta_ROI (Kelly - Flat) across all 12 cells: %+.5f ",
            mean(merged$Delta_ROI, na.rm = TRUE)))
cat(sprintf("(positive in %d / %d cells).\n",
            sum(merged$Delta_ROI > 0, na.rm = TRUE), nrow(merged)))

overall_cap <- sum(merged$Kelly_cap_hit_pct * merged$N, na.rm = TRUE) /
               sum(merged$N, na.rm = TRUE)
overall_anom <- sum(merged$Kelly_anomalous_n, na.rm = TRUE)
cat(sprintf("6. Overall cap-hit rate (N-weighted): %.2f%%; anomalous f <= 0 flagged: %d bets.\n",
            overall_cap, overall_anom))
