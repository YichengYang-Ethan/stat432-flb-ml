# 14_pnl_and_tables.R
# Regime-conditional strategies (A,B,C), cumulative P&L figure, headline table.

# -----------------------------------------------------------------------------
# From 02_regime_conditional.R
# -----------------------------------------------------------------------------

# Section 5.3 - Regime-conditional strategies.
#   A: Regime 1 only
#   B: Regime 1 + Regime 2
#   C: Regime 3 + Regime 4  (control)

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

matches_per_year <- 3500

# -- Metric helpers -------------------------------------------------------
max_drawdown <- function(cum_pnl) {
  peak <- cummax(cum_pnl)
  dd   <- peak - cum_pnl
  list(max_abs = max(dd),
       max_pct = max(dd / pmax(peak, 1)) * 100)
}

eval_bets <- function(df_placed, model_name, strategy_name) {
  if (nrow(df_placed) == 0) {
    return(tibble(
      Strategy = strategy_name, Model = model_name,
      N = 0L, Avg_odds = NA_real_, Win_rate = NA_real_,
      Total_profit = 0, ROI = NA_real_, Sharpe = NA_real_,
      MaxDD_abs = NA_real_, MaxDD_pct = NA_real_
    ))
  }
  n        <- nrow(df_placed)
  tot_prof <- sum(df_placed$profit)
  roi      <- tot_prof / n
  wins     <- sum(df_placed$won)
  win_rate <- wins / n
  avg_odds <- mean(df_placed$bet_odds)
  sd_prof  <- sd(df_placed$profit)
  sharpe   <- if (!is.na(sd_prof) && sd_prof > 0)
    (tot_prof / n) / sd_prof * sqrt(matches_per_year) else NA_real_

  chr <- df_placed %>% arrange(Date)
  cum <- cumsum(chr$profit)
  dd  <- max_drawdown(cum)

  tibble(Strategy = strategy_name, Model = model_name,
         N = n, Avg_odds = avg_odds, Win_rate = win_rate,
         Total_profit = tot_prof, ROI = roi, Sharpe = sharpe,
         MaxDD_abs = dd$max_abs, MaxDD_pct = dd$max_pct)
}

models <- list(
  list(name = "Shin (1993)", file = "bets_shin.rds"),
  list(name = "Elastic Net", file = "bets_en.rds"),
  list(name = "XGBoost",     file = "bets_xgb.rds")
)

strategies <- list(
  list(name = "Unconditional",      regimes = as.character(1:4)),
  list(name = "A: Regime 1",        regimes = "1"),
  list(name = "B: Regime 1+2",      regimes = c("1","2")),
  list(name = "C: Regime 3+4 (ctl)", regimes = c("3","4"))
)

res <- map_dfr(models, function(m) {
  df <- readRDS(file.path(data_dir, m$file))
  placed <- df %>% filter(bet_placed)
  map_dfr(strategies, function(s) {
    sub <- placed %>% filter(as.character(Regime) %in% s$regimes)
    eval_bets(sub, m$name, s$name)
  })
}) %>%
  arrange(Model, Strategy)

# -- Save table -----------------------------------------------------------
res_out <- res %>% mutate(across(where(is.numeric), ~ signif(., 4)))
write_csv(res_out, file.path(tbl_dir, "table_5_2_regime_strategies.csv"))

tex <- kable(res_out, format = "latex", booktabs = TRUE, longtable = TRUE,
             caption = "Regime-conditional betting strategies (Pinnacle closing, flat 1-unit stakes).",
             label = "tab:regime-strategies") %>%
  kable_styling(latex_options = c("repeat_header"), full_width = FALSE)
writeLines(as.character(tex), file.path(tbl_dir, "table_5_2_regime_strategies.tex"))

# -- Figure 5.1: cumulative P&L per model, 4 lines per panel --------------
build_cum_df <- function(m) {
  df <- readRDS(file.path(data_dir, m$file)) %>% filter(bet_placed)
  map_dfr(strategies, function(s) {
    sub <- df %>% filter(as.character(Regime) %in% s$regimes) %>% arrange(Date)
    if (nrow(sub) == 0) return(tibble(Model = m$name, Strategy = s$name,
                                      Date = as.Date(character()), cum_pnl = double()))
    tibble(Model = m$name, Strategy = s$name,
           Date = sub$Date, cum_pnl = cumsum(sub$profit))
  })
}

cum_df <- map_dfr(models, build_cum_df)
cum_df <- cum_df %>%
  mutate(Model = factor(Model, levels = sapply(models, function(m) m$name)),
         Strategy = factor(Strategy, levels = sapply(strategies, function(s) s$name)))

strategy_colours <- c("Unconditional"       = "grey50",
                      "A: Regime 1"         = "#2c7fb8",
                      "B: Regime 1+2"       = "#1b7837",
                      "C: Regime 3+4 (ctl)" = "#b2182b")
strategy_linetypes <- c("Unconditional"       = "dashed",
                        "A: Regime 1"         = "solid",
                        "B: Regime 1+2"       = "solid",
                        "C: Regime 3+4 (ctl)" = "solid")

p_pnl <- ggplot(cum_df, aes(x = Date, y = cum_pnl,
                            colour = Strategy, linetype = Strategy)) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.3) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = strategy_colours) +
  scale_linetype_manual(values = strategy_linetypes) +
  facet_wrap(~ Model, ncol = 1, scales = "free_y") +
  labs(x = "Date",
       y = "Cumulative profit (units)",
       title = "Figure 5.1 - Cumulative P&L by model and strategy",
       subtitle = "Pinnacle closing, flat 1-unit stakes; horizontal line at 0") +
  theme_eda
save_png(p_pnl, "figure_5_1_cumulative_pnl", w = 9, h = 9)

# -- Console findings -----------------------------------------------------
cat("==== Section 5.3 findings ====\n")
cat("1. Regime-conditional table:\n")
print(res_out, n = Inf)

a_only <- res %>% filter(Strategy == "A: Regime 1") %>% arrange(Model)
cat("\n2. Strategy A (Regime 1 only) ROI per model:\n")
for (i in seq_len(nrow(a_only))) {
  cat(sprintf("   %-12s: ROI = %+.4f  (n = %s, win rate = %.1f%%, avg odds = %.2f, total profit %+.1f).\n",
              a_only$Model[i], a_only$ROI[i],
              format(a_only$N[i], big.mark = ","),
              a_only$Win_rate[i] * 100,
              a_only$Avg_odds[i],
              a_only$Total_profit[i]))
}

pos_combos <- res %>% filter(ROI > 0) %>% arrange(desc(ROI))
if (nrow(pos_combos) > 0) {
  cat(sprintf("\n3. %d (model, strategy) combinations achieve POSITIVE ROI:\n",
              nrow(pos_combos)))
  print(pos_combos %>% select(Strategy, Model, N, ROI, Sharpe) %>%
          mutate(across(where(is.numeric), ~ signif(., 4))))
} else {
  cat("\n3. NO (model, strategy) combination achieves positive ROI.\n")
}

cat(sprintf("\n4. Best ROI overall: %s / %s at %+.4f.\n",
            res$Model[which.max(res$ROI)],
            res$Strategy[which.max(res$ROI)],
            max(res$ROI)))
cat(sprintf("5. Worst ROI overall: %s / %s at %+.4f.\n",
            res$Model[which.min(res$ROI)],
            res$Strategy[which.min(res$ROI)],
            min(res$ROI)))

# -----------------------------------------------------------------------------
# From 04_summary.R
# -----------------------------------------------------------------------------

# Consolidate headline numbers from 01 and 02 into one table.

suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
})

root     <- "~/Desktop/FLB_Final_Code"
tbl_dir  <- file.path(root, "tables")

uncond <- read_csv(file.path(tbl_dir, "table_5_1_unconditional.csv"),
                   show_col_types = FALSE)
regime <- read_csv(file.path(tbl_dir, "table_5_2_regime_strategies.csv"),
                   show_col_types = FALSE)

# The regime table already has Unconditional rows. Keep one source of truth:
# use the regime table and restrict to the columns we want.
headline <- regime %>%
  select(Strategy, Model, N, ROI, Sharpe, MaxDD_pct) %>%
  mutate(
    # Format-friendly ordering: Unconditional first, then A, B, C
    Strategy = factor(Strategy,
                      levels = c("Unconditional",
                                 "A: Regime 1",
                                 "B: Regime 1+2",
                                 "C: Regime 3+4 (ctl)"))
  ) %>%
  arrange(Strategy, Model)

write_csv(headline %>% mutate(across(where(is.numeric), ~ signif(., 4))),
          file.path(tbl_dir, "table_5_6_headline.csv"))

tex <- kable(headline %>% mutate(across(where(is.numeric), ~ signif(., 4))),
             format = "latex", booktabs = TRUE, longtable = TRUE,
             caption = "Headline betting results: 3 strategies x 3 models, Pinnacle closing, flat 1-unit stakes.",
             label = "tab:headline-betting") %>%
  kable_styling(latex_options = c("repeat_header"), full_width = FALSE)
writeLines(as.character(tex), file.path(tbl_dir, "table_5_6_headline.tex"))

cat("==== Section 5.5 headline ====\n")
print(headline %>% mutate(across(where(is.numeric), ~ signif(., 4))), n = Inf)

best_roi  <- headline %>% slice_max(ROI, n = 1)
worst_roi <- headline %>% slice_min(ROI, n = 1)

cat(sprintf("\nBest ROI overall: %s / %s at %+.4f.\n",
            best_roi$Model, as.character(best_roi$Strategy), best_roi$ROI))
cat(sprintf("Worst ROI overall: %s / %s at %+.4f.\n",
            worst_roi$Model, as.character(worst_roi$Strategy), worst_roi$ROI))

any_pos <- headline %>% filter(ROI > 0)
cat(sprintf("\n(model, strategy) combos with positive ROI: %d.\n", nrow(any_pos)))
if (nrow(any_pos) > 0) print(any_pos)
