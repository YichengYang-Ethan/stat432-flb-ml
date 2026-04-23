# 13_betting_strategies.R
# Construct bet ledgers at Pinnacle closing + unconditional ROI.

# -----------------------------------------------------------------------------
# From 00_construct_bets.R
# -----------------------------------------------------------------------------

# Section 5.1 - Strategy construction.
# For each model and each test match, compute EV_k for k in {H,D,A} using
# Pinnacle closing odds, pick the highest-EV outcome, and bet it if EV > 0.

suppressPackageStartupMessages({
  library(tidyverse)
})

root     <- "~/Desktop/FLB_Final_Code"
data_dir <- file.path(root, "data")

s4_data <- "~/Desktop/FLB_Final_Code/data"
test     <- readRDS(file.path(s4_data, "test_features.rds"))
baseline <- readRDS(file.path(s4_data, "baseline_preds_test.rds"))
en       <- readRDS(file.path(s4_data, "elasticnet_preds_test.rds"))
xgb      <- readRDS(file.path(s4_data, "xgb_preds_test.rds"))

cat("test rows:", nrow(test), "\n")

# -- Join predictions to odds by match_id ---------------------------------
odds_tbl <- test %>%
  select(match_id, Date, Regime, HomeTeam, AwayTeam, FTR,
         PSCH, PSCD, PSCA,
         B365H, B365D, B365A,
         BWH, BWD, BWA,
         IWH, IWD, IWA,
         WHH, WHD, WHA,
         VCH, VCD, VCA)

build_for_model <- function(preds, p_prefix) {
  joined <- odds_tbl %>%
    left_join(
      preds %>% select(match_id,
                       !!sym(paste0(p_prefix, "H")),
                       !!sym(paste0(p_prefix, "D")),
                       !!sym(paste0(p_prefix, "A"))),
      by = "match_id"
    )
  # Rename p columns to pH, pD, pA
  joined <- joined %>%
    rename(pH = !!sym(paste0(p_prefix, "H")),
           pD = !!sym(paste0(p_prefix, "D")),
           pA = !!sym(paste0(p_prefix, "A")))
  stopifnot(all(!is.na(joined$pH)))
  joined
}

construct_bets <- function(df) {
  # Pinnacle closing odds are the settlement price
  df <- df %>%
    mutate(
      EV_H = pH * PSCH - 1,
      EV_D = pD * PSCD - 1,
      EV_A = pA * PSCA - 1
    )

  # Best-EV outcome per row (H=1, D=2, A=3)
  EV_mat <- as.matrix(df[, c("EV_H","EV_D","EV_A")])
  best_idx   <- max.col(EV_mat, ties.method = "first")
  best_EV    <- EV_mat[cbind(seq_len(nrow(EV_mat)), best_idx)]
  outcomes   <- c("H","D","A")
  bet_outcome <- outcomes[best_idx]
  bet_odds_H <- df$PSCH; bet_odds_D <- df$PSCD; bet_odds_A <- df$PSCA
  bet_odds <- ifelse(bet_outcome == "H", bet_odds_H,
              ifelse(bet_outcome == "D", bet_odds_D, bet_odds_A))
  p_model <- ifelse(bet_outcome == "H", df$pH,
             ifelse(bet_outcome == "D", df$pD, df$pA))

  bet_placed <- best_EV > 0
  won <- bet_placed & (bet_outcome == as.character(df$FTR))
  profit <- ifelse(bet_placed, ifelse(won, bet_odds - 1, -1), 0)

  tibble(
    match_id    = df$match_id,
    Date        = df$Date,
    HomeTeam    = df$HomeTeam,
    AwayTeam    = df$AwayTeam,
    Regime      = df$Regime,
    FTR         = df$FTR,
    bet_placed  = bet_placed,
    bet_outcome = bet_outcome,
    bet_odds    = bet_odds,
    p_model     = p_model,
    EV          = best_EV,
    won         = as.integer(won),
    profit      = profit
  )
}

df_shin <- build_for_model(baseline, "p_shin_")
df_en   <- build_for_model(en,       "p_en_")
df_xgb  <- build_for_model(xgb,      "p_xgb_")

bets_shin <- construct_bets(df_shin)
bets_en   <- construct_bets(df_en)
bets_xgb  <- construct_bets(df_xgb)

saveRDS(bets_shin, file.path(data_dir, "bets_shin.rds"))
saveRDS(bets_en,   file.path(data_dir, "bets_en.rds"))
saveRDS(bets_xgb,  file.path(data_dir, "bets_xgb.rds"))

# -- Summary ---------------------------------------------------------------
summarize_bets <- function(df, name) {
  placed <- df %>% filter(bet_placed)
  n_all <- nrow(df)
  n_placed <- nrow(placed)
  tibble(
    model      = name,
    n_test     = n_all,
    n_bet      = n_placed,
    pct_bet    = n_placed / n_all * 100,
    mean_EV    = mean(placed$EV),
    median_EV  = median(placed$EV),
    mean_odds  = mean(placed$bet_odds)
  )
}

summary_tbl <- bind_rows(
  summarize_bets(bets_shin, "Shin (1993)"),
  summarize_bets(bets_en,   "Elastic Net"),
  summarize_bets(bets_xgb,  "XGBoost")
)

cat("\n==== Section 5.1 findings ====\n")
cat("1. Bet-construction summary (EV > 0 filter, Pinnacle closing odds):\n")
print(summary_tbl %>% mutate(across(where(is.numeric), ~ signif(., 4))))

for (i in seq_len(nrow(summary_tbl))) {
  cat(sprintf("%d. %-12s: %s of %s matches would be bet (%.1f%%); mean EV = %+.4f.\n",
              i + 1,
              summary_tbl$model[i],
              format(summary_tbl$n_bet[i], big.mark = ","),
              format(summary_tbl$n_test[i], big.mark = ","),
              summary_tbl$pct_bet[i],
              summary_tbl$mean_EV[i]))
}
cat(sprintf("5. Pinnacle closing used for settlement (PSCH/D/A); sensitivity to max-odds in 03.\n"))

# -----------------------------------------------------------------------------
# From 01_unconditional.R
# -----------------------------------------------------------------------------

# Section 5.2 - Unconditional betting benchmark.
# Bet every match where model EV > 0 (already filtered in the ledgers).

suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
})

root     <- "~/Desktop/FLB_Final_Code"
data_dir <- file.path(root, "data")
tbl_dir  <- file.path(root, "tables")

matches_per_year <- 3500  # 11 leagues x ~320 matches/league/year

# -- Metric helpers -------------------------------------------------------
max_drawdown <- function(cum_pnl) {
  # Starting bankroll = 0; running peak = max(cum_pnl[1..t]).
  # Absolute drawdown = peak - cum_pnl[t]; take max over t.
  peak <- cummax(cum_pnl)
  dd   <- peak - cum_pnl
  max_abs_dd <- max(dd)
  # Peak may be 0 at start; normalize by pmax(peak, 1) so % is bounded.
  max_pct <- max(dd / pmax(peak, 1)) * 100
  list(max_abs = max_abs_dd, max_pct = max_pct)
}

eval_bets <- function(df_placed, model_name, strategy_name = "Unconditional") {
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
  sharpe   <- if (sd_prof > 0) (tot_prof / n) / sd_prof * sqrt(matches_per_year) else NA_real_

  chr <- df_placed %>% arrange(Date)
  cum <- cumsum(chr$profit)
  dd  <- max_drawdown(cum)

  tibble(
    Strategy = strategy_name, Model = model_name,
    N = n, Avg_odds = avg_odds, Win_rate = win_rate,
    Total_profit = tot_prof, ROI = roi, Sharpe = sharpe,
    MaxDD_abs = dd$max_abs, MaxDD_pct = dd$max_pct
  )
}

models <- list(
  list(name = "Shin (1993)", file = "bets_shin.rds"),
  list(name = "Elastic Net", file = "bets_en.rds"),
  list(name = "XGBoost",     file = "bets_xgb.rds")
)

res <- map_dfr(models, function(m) {
  df <- readRDS(file.path(data_dir, m$file))
  placed <- df %>% filter(bet_placed)
  eval_bets(placed, m$name, "Unconditional")
})

res_out <- res %>% mutate(across(where(is.numeric), ~ signif(., 4)))
write_csv(res_out, file.path(tbl_dir, "table_5_1_unconditional.csv"))

tex <- kable(res_out, format = "latex", booktabs = TRUE,
             caption = "Unconditional betting performance on the test set (Pinnacle closing, flat 1-unit stakes, EV > 0 filter).",
             label = "tab:unconditional-betting") %>%
  kable_styling(latex_options = c("hold_position"))
writeLines(as.character(tex), file.path(tbl_dir, "table_5_1_unconditional.tex"))

cat("==== Section 5.2 findings ====\n")
cat("1. Unconditional betting performance:\n")
print(res_out)

for (i in seq_len(nrow(res))) {
  cat(sprintf("%d. %-12s unconditional ROI: %+.4f  (n = %s bets, total profit %+.1f units).\n",
              i + 1,
              res$Model[i],
              res$ROI[i],
              format(res$N[i], big.mark = ","),
              res$Total_profit[i]))
}
cat(sprintf("5. Best unconditional ROI: %s (%+.4f).  Worst: %s (%+.4f).\n",
            res$Model[which.max(res$ROI)], max(res$ROI),
            res$Model[which.min(res$ROI)], min(res$ROI)))
