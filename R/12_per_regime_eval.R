# 12_per_regime_eval.R
# Overall 5-model comparison and regime-conditional evaluation.

# -----------------------------------------------------------------------------
# From 06_overall_eval.R
# -----------------------------------------------------------------------------

# Section 4.5 - pool all five test-set predictors and compare.

set.seed(441)
suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
  library(pROC)
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

baseline <- readRDS(file.path(data_dir, "baseline_preds_test.rds"))
en       <- readRDS(file.path(data_dir, "elasticnet_preds_test.rds"))
rf       <- readRDS(file.path(data_dir, "rf_preds_test.rds"))
xgb      <- readRDS(file.path(data_dir, "xgb_preds_test.rds"))

# Join everything by match_id -------------------------------------------
all_preds <- baseline %>%
  left_join(en  %>% select(match_id, p_en_H, p_en_D, p_en_A),  by = "match_id") %>%
  left_join(rf  %>% select(match_id, p_rf_H, p_rf_D, p_rf_A),  by = "match_id") %>%
  left_join(xgb %>% select(match_id, p_xgb_H, p_xgb_D, p_xgb_A), by = "match_id")

stopifnot(nrow(all_preds) == nrow(baseline))
stopifnot(all(!is.na(all_preds$p_xgb_H)))

# Model specs ------------------------------------------------------------
models <- list(
  list(name = "Naive baseline",     cols = c("p_naive_H","p_naive_D","p_naive_A")),
  list(name = "Shin (1993)",        cols = c("p_shin_H", "p_shin_D", "p_shin_A")),
  list(name = "Elastic Net",        cols = c("p_en_H",   "p_en_D",   "p_en_A")),
  list(name = "Random Forest",      cols = c("p_rf_H",   "p_rf_D",   "p_rf_A")),
  list(name = "XGBoost",            cols = c("p_xgb_H",  "p_xgb_D",  "p_xgb_A"))
)

# Metrics ---------------------------------------------------------------
brier_mult <- function(P, y) {
  Y <- outer(as.character(y), colnames(P), `==`) * 1L
  mean(rowSums((P - Y)^2))
}
log_loss <- function(P, y) {
  y_int <- match(as.character(y), colnames(P))
  -mean(log(pmax(P[cbind(seq_len(nrow(P)), y_int)], 1e-15)))
}
macro_auc <- function(P, y) {
  levs <- colnames(P)
  aucs <- sapply(levs, function(k) {
    y_bin <- as.integer(as.character(y) == k)
    if (length(unique(y_bin)) < 2) return(NA_real_)
    as.numeric(auc(roc(y_bin, P[, k], quiet = TRUE)))
  })
  mean(aucs, na.rm = TRUE)
}

y_test <- all_preds$FTR

res <- map_dfr(models, function(m) {
  P <- as.matrix(all_preds[, m$cols]); colnames(P) <- c("H","D","A")
  tibble(Model = m$name,
         Brier = brier_mult(P, y_test),
         LogLoss = log_loss(P, y_test),
         MacroAUC = macro_auc(P, y_test))
})

# Calibration slope per model -------------------------------------------
calibration_slope <- function(P, y) {
  df <- bind_rows(
    tibble(p = P[, "H"], y = as.integer(as.character(y) == "H")),
    tibble(p = P[, "D"], y = as.integer(as.character(y) == "D")),
    tibble(p = P[, "A"], y = as.integer(as.character(y) == "A"))
  )
  breaks <- seq(0, 1, length.out = 21)
  bin_tbl <- df %>%
    mutate(bin = cut(p, breaks = breaks, include.lowest = TRUE, right = FALSE)) %>%
    group_by(bin) %>%
    summarise(mean_p = mean(p), realized = mean(y), n = n(), .groups = "drop") %>%
    filter(n > 0)
  fit <- lm(realized ~ mean_p, data = bin_tbl, weights = n)
  list(slope = unname(coef(fit)[2]), bins = bin_tbl %>% mutate(model_p = mean_p))
}

cal_list <- map(models, function(m) {
  P <- as.matrix(all_preds[, m$cols]); colnames(P) <- c("H","D","A")
  list(name = m$name, cal = calibration_slope(P, y_test))
})

res$CalibSlope <- sapply(cal_list, function(x) x$cal$slope)

write_csv(res %>% mutate(across(where(is.numeric), ~ signif(., 4))),
          file.path(tbl_dir, "table_4_6_overall_comparison.csv"))

tex <- kable(res %>% mutate(across(where(is.numeric), ~ signif(., 4))),
             format = "latex", booktabs = TRUE,
             caption = "Overall test-set comparison across 2 baselines and 3 ML models.",
             label = "tab:overall-comparison") %>%
  kable_styling(latex_options = c("hold_position"))
writeLines(as.character(tex), file.path(tbl_dir, "table_4_6_overall_comparison.tex"))

# Figure 4.3: calibration overlay ---------------------------------------
cal_df <- map_dfr(cal_list, function(x) {
  x$cal$bins %>% mutate(Model = x$name,
                        slope = x$cal$slope)
})
cal_df <- cal_df %>%
  mutate(Model = factor(Model, levels = sapply(models, function(m) m$name)))

p_cal <- ggplot(cal_df, aes(x = mean_p, y = realized, colour = Model)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey30") +
  geom_line(linewidth = 0.6) +
  geom_point(aes(size = n), alpha = 0.6) +
  scale_size_continuous(range = c(0.5, 3), guide = "none") +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Predicted probability",
       y = "Realized frequency",
       title = "Figure 4.3 - Calibration overlay (all 5 models)",
       subtitle = "Dashed line: perfect calibration") +
  theme_eda
save_png(p_cal, "figure_4_3_calibration_overlay", w = 8, h = 6)

cat("==== Section 4.5 findings ====\n")
cat("1. Overall comparison:\n")
print(res %>% mutate(across(where(is.numeric), ~ signif(., 4))))
cat(sprintf("2. Lowest Brier: %s (%.5f).\n",
            res$Model[which.min(res$Brier)], min(res$Brier)))
cat(sprintf("3. Lowest LogLoss: %s (%.5f).\n",
            res$Model[which.min(res$LogLoss)], min(res$LogLoss)))
cat(sprintf("4. Highest Macro-AUC: %s (%.5f).\n",
            res$Model[which.max(res$MacroAUC)], max(res$MacroAUC)))
cat(sprintf("5. Best-calibrated (slope closest to 1): %s (slope = %.3f).\n",
            res$Model[which.min(abs(res$CalibSlope - 1))],
            res$CalibSlope[which.min(abs(res$CalibSlope - 1))]))

# -----------------------------------------------------------------------------
# From 07_per_regime_eval.R
# -----------------------------------------------------------------------------

# Section 4.6 - per-regime model comparison: does any ML model beat
# BOTH baselines on the high-disagreement regime?

set.seed(441)
suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
  library(pROC)
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

baseline <- readRDS(file.path(data_dir, "baseline_preds_test.rds"))
en       <- readRDS(file.path(data_dir, "elasticnet_preds_test.rds"))
rf       <- readRDS(file.path(data_dir, "rf_preds_test.rds"))
xgb      <- readRDS(file.path(data_dir, "xgb_preds_test.rds"))

all_preds <- baseline %>%
  left_join(en  %>% select(match_id, p_en_H, p_en_D, p_en_A),  by = "match_id") %>%
  left_join(rf  %>% select(match_id, p_rf_H, p_rf_D, p_rf_A),  by = "match_id") %>%
  left_join(xgb %>% select(match_id, p_xgb_H, p_xgb_D, p_xgb_A), by = "match_id")

models <- list(
  list(name = "Naive baseline", cols = c("p_naive_H","p_naive_D","p_naive_A")),
  list(name = "Shin (1993)",    cols = c("p_shin_H", "p_shin_D", "p_shin_A")),
  list(name = "Elastic Net",    cols = c("p_en_H",   "p_en_D",   "p_en_A")),
  list(name = "Random Forest",  cols = c("p_rf_H",   "p_rf_D",   "p_rf_A")),
  list(name = "XGBoost",        cols = c("p_xgb_H",  "p_xgb_D",  "p_xgb_A"))
)

brier_mult <- function(P, y) {
  Y <- outer(as.character(y), colnames(P), `==`) * 1L
  mean(rowSums((P - Y)^2))
}
log_loss <- function(P, y) {
  y_int <- match(as.character(y), colnames(P))
  -mean(log(pmax(P[cbind(seq_len(nrow(P)), y_int)], 1e-15)))
}
macro_auc <- function(P, y) {
  levs <- colnames(P)
  aucs <- sapply(levs, function(k) {
    y_bin <- as.integer(as.character(y) == k)
    if (length(unique(y_bin)) < 2) return(NA_real_)
    as.numeric(auc(roc(y_bin, P[, k], quiet = TRUE)))
  })
  mean(aucs, na.rm = TRUE)
}

compute_metrics <- function(df) {
  y <- df$FTR
  map_dfr(models, function(m) {
    P <- as.matrix(df[, m$cols]); colnames(P) <- c("H","D","A")
    tibble(Model = m$name,
           Brier = brier_mult(P, y),
           LogLoss = log_loss(P, y),
           MacroAUC = macro_auc(P, y),
           n = nrow(df))
  })
}

# Per-regime metrics -----------------------------------------------------
regimes <- sort(unique(all_preds$Regime))
per_regime <- map_dfr(regimes, function(r) {
  sub <- all_preds %>% filter(Regime == r)
  compute_metrics(sub) %>% mutate(Regime = r)
}) %>%
  relocate(Regime, Model)

# Attach "lift vs Naive" for each (Regime, Model) ------------------------
naive_ref <- per_regime %>%
  filter(Model == "Naive baseline") %>%
  select(Regime, Naive_Brier = Brier, Naive_LogLoss = LogLoss)

per_regime <- per_regime %>%
  left_join(naive_ref, by = "Regime") %>%
  mutate(BrierLift   = (Naive_Brier   - Brier)   / Naive_Brier,
         LogLossLift = (Naive_LogLoss - LogLoss) / Naive_LogLoss) %>%
  select(-Naive_Brier, -Naive_LogLoss)

# Sort: by regime, then Brier ascending
per_regime <- per_regime %>% arrange(Regime, Brier)

write_csv(per_regime %>% mutate(across(where(is.numeric), ~ signif(., 4))),
          file.path(tbl_dir, "table_4_7_per_regime.csv"))

tex <- kable(per_regime %>% mutate(across(where(is.numeric), ~ signif(., 4))),
             format = "latex", booktabs = TRUE, longtable = TRUE,
             caption = "Per-regime model comparison (test set). Lift = improvement over Naive baseline.",
             label = "tab:per-regime") %>%
  kable_styling(latex_options = c("repeat_header"), full_width = FALSE)
writeLines(as.character(tex), file.path(tbl_dir, "table_4_7_per_regime.tex"))

# Winning model per regime -----------------------------------------------
winners <- per_regime %>%
  group_by(Regime) %>%
  summarise(
    best_brier_model   = Model[which.min(Brier)],
    best_brier_value   = min(Brier),
    best_logloss_model = Model[which.min(LogLoss)],
    best_logloss_value = min(LogLoss),
    naive_brier        = Brier[Model == "Naive baseline"],
    naive_logloss      = LogLoss[Model == "Naive baseline"],
    .groups = "drop"
  ) %>%
  mutate(
    brier_improves_naive   = best_brier_value   < naive_brier,
    logloss_improves_naive = best_logloss_value < naive_logloss
  )

write_csv(winners %>% mutate(across(where(is.numeric), ~ signif(., 4))),
          file.path(tbl_dir, "table_4_8_winning_model_per_regime.csv"))

tex <- kable(winners %>% mutate(across(where(is.numeric), ~ signif(., 4))),
             format = "latex", booktabs = TRUE, longtable = TRUE,
             caption = "Winning model per regime (test set): lowest Brier and lowest LogLoss, plus Naive reference.",
             label = "tab:per-regime-winners") %>%
  kable_styling(latex_options = c("repeat_header"), full_width = FALSE)
writeLines(as.character(tex), file.path(tbl_dir, "table_4_8_winning_model_per_regime.tex"))

# Figure 4.4: grouped bars per regime -------------------------------------
plot_df <- per_regime %>%
  mutate(Model = factor(Model, levels = sapply(models, function(m) m$name)))
naive_line <- per_regime %>% filter(Model == "Naive baseline") %>%
  select(Regime, naive_brier = Brier)

p4 <- ggplot(plot_df, aes(x = factor(Regime), y = Brier, fill = Model)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_segment(data = naive_line,
               aes(x = as.numeric(factor(Regime)) - 0.5,
                   xend = as.numeric(factor(Regime)) + 0.5,
                   y = naive_brier, yend = naive_brier),
               inherit.aes = FALSE,
               linetype = "dashed", colour = "grey20") +
  scale_fill_brewer(palette = "Set2") +
  coord_cartesian(ylim = c(0.30, 0.75)) +
  labs(x = "Regime",
       y = "Brier score (test set; lower is better)",
       title = "Figure 4.4 - Per-regime Brier across 5 models",
       subtitle = "Dashed horizontal segments: Naive-baseline Brier per regime") +
  theme_eda
save_png(p4, "figure_4_4_per_regime_brier", w = 10, h = 6)

# -- Findings -------------------------------------------------------------
cat("==== Section 4.6 findings ====\n")
cat("1. Per-regime table (sorted by Brier within regime):\n")
print(per_regime %>% mutate(across(where(is.numeric), ~ signif(., 4))), n = Inf)
cat("\n2. Winning model per regime (Brier / LogLoss):\n")
print(winners %>% mutate(across(where(is.numeric), ~ signif(., 4))))

# RQ2: regime 1 is the high-disagreement regime from Section 3
r1 <- per_regime %>% filter(Regime == "1")
r1_ml <- r1 %>% filter(!Model %in% c("Naive baseline", "Shin (1993)"))
r1_base_min <- min(r1$Brier[r1$Model %in% c("Naive baseline", "Shin (1993)")])
beat_both <- r1_ml %>% filter(Brier < r1_base_min)

cat(sprintf("\n3. Regime 1 (Wild West) baseline best Brier (Naive/Shin): %.5f.\n",
            r1_base_min))
if (nrow(beat_both) > 0) {
  cat(sprintf("4. In Regime 1, %d ML model(s) beat BOTH baselines on Brier:\n",
              nrow(beat_both)))
  print(beat_both %>% select(Model, Brier, BrierLift))
} else {
  cat("4. In Regime 1, NO ML model beats both baselines on Brier.\n")
}

# Regime with the largest ML-over-baseline improvement
gain_by_regime <- per_regime %>%
  filter(!Model %in% c("Naive baseline", "Shin (1993)")) %>%
  group_by(Regime) %>%
  summarise(best_ml_brier = min(Brier), .groups = "drop") %>%
  left_join(naive_ref %>% transmute(Regime, naive_brier = Naive_Brier), by = "Regime") %>%
  mutate(lift = (naive_brier - best_ml_brier) / naive_brier) %>%
  arrange(desc(lift))
cat("\n5. Best ML lift (vs Naive) by regime:\n")
print(gain_by_regime %>% mutate(across(where(is.numeric), ~ signif(., 4))))
