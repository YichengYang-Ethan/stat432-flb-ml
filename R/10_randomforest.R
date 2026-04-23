# 04_randomforest.R
# Section 4.4a - Random Forest with ranger (probability forest).

set.seed(441)
suppressPackageStartupMessages({
  library(tidyverse)
  library(ranger)
  library(kableExtra)
  library(pROC)
})

root      <- "~/Desktop/FLB_Final_Code"
data_dir  <- file.path(root, "data")
fig_dir   <- file.path(root, "figures")
tbl_dir   <- file.path(root, "tables")
model_dir <- file.path(root, "models")

theme_eda <- theme_bw() + theme(panel.grid.minor = element_blank())
save_png <- function(p, name, w = 8, h = 5) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p,
         width = w, height = h, dpi = 300)
}

train <- readRDS(file.path(data_dir, "train_features.rds"))
test  <- readRDS(file.path(data_dir, "test_features.rds"))
scaling <- readRDS(file.path(data_dir, "feature_scaling.rds"))
cont_feats <- scaling$cont_feats

build_df <- function(df) df %>% select(FTR, all_of(cont_feats), Regime)

train_df <- build_df(train)
test_df  <- build_df(test)

cat("Training Random Forest (ranger, 500 trees, mtry = 6) ...\n")
rf <- ranger(
  formula      = FTR ~ .,
  data         = train_df,
  num.trees    = 500,
  mtry         = 6,
  min.node.size = 10,
  probability  = TRUE,
  importance   = "impurity",
  seed         = 441,
  num.threads  = 1
)
saveRDS(rf, file.path(model_dir, "rf_fit.rds"))

# Predict ----------------------------------------------------------------
P <- predict(rf, data = test_df, num.threads = 1)$predictions
# Reorder columns to H, D, A
P <- P[, c("H","D","A"), drop = FALSE]
colnames(P) <- c("H","D","A")

# Metrics ----------------------------------------------------------------
log_loss <- function(P, y) {
  y_int <- match(as.character(y), colnames(P))
  p_true <- P[cbind(seq_len(nrow(P)), y_int)]
  -mean(log(pmax(p_true, 1e-15)))
}
brier_mult <- function(P, y) {
  Y <- outer(as.character(y), colnames(P), `==`) * 1L
  mean(rowSums((P - Y)^2))
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

brier_rf <- brier_mult(P, test$FTR)
ll_rf    <- log_loss(P,   test$FTR)
auc_rf   <- macro_auc(P,  test$FTR)

# Save preds -------------------------------------------------------------
preds <- tibble(
  match_id = test$match_id,
  FTR      = test$FTR,
  Regime   = test$Regime,
  Date     = test$Date,
  Div      = test$Div,
  p_rf_H = P[, "H"], p_rf_D = P[, "D"], p_rf_A = P[, "A"]
)
saveRDS(preds, file.path(data_dir, "rf_preds_test.rds"))

# Importance -------------------------------------------------------------
imp <- tibble(feature = names(rf$variable.importance),
              impurity = as.numeric(rf$variable.importance)) %>%
  arrange(desc(impurity))

write_csv(imp %>% mutate(impurity = signif(impurity, 4)),
          file.path(tbl_dir, "table_4_4_rf_importance.csv"))
tex <- kable(imp %>% mutate(impurity = signif(impurity, 4)),
             format = "latex", booktabs = TRUE, longtable = TRUE,
             caption = "Random Forest feature importance (Gini impurity).",
             label = "tab:rf-importance") %>%
  kable_styling(latex_options = c("repeat_header"), full_width = FALSE)
writeLines(as.character(tex), file.path(tbl_dir, "table_4_4_rf_importance.tex"))

# Figure 4.1 -------------------------------------------------------------
top_imp <- imp %>% slice_head(n = 15)
p_imp <- ggplot(top_imp, aes(x = reorder(feature, impurity), y = impurity)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(x = NULL, y = "Impurity importance",
       title = "Figure 4.1 - Random Forest: top 15 features",
       subtitle = sprintf("ranger, 500 trees, mtry = 6; test Brier = %.4f", brier_rf)) +
  theme_eda
save_png(p_imp, "figure_4_1_rf_importance", w = 8, h = 5)

cat("\n==== Section 4.4a findings ====\n")
cat(sprintf("1. Test Brier   = %.5f.\n", brier_rf))
cat(sprintf("2. Test LogLoss = %.5f.\n", ll_rf))
cat(sprintf("3. Test Macro-AUC = %.5f.\n", auc_rf))
cat("4. Top 5 features by impurity importance:\n")
print(imp %>% slice_head(n = 5))
cat(sprintf("5. OOB prediction error (ranger internal) = %.5f.\n", rf$prediction.error))
