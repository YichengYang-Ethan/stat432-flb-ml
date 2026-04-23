# 05_xgboost.R
# Section 4.4b - Gradient boosting via xgboost.

set.seed(441)
suppressPackageStartupMessages({
  library(tidyverse)
  library(xgboost)
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

# Design: continuous feats + Regime one-hot (3 dummies)
build_design <- function(df) {
  X_cont <- as.matrix(df[, cont_feats])
  R <- model.matrix(~ Regime - 1, data = df)[, -1, drop = FALSE]
  colnames(R) <- paste0("Regime_", sub("^Regime", "", colnames(R)))
  cbind(X_cont, R)
}

X_train <- build_design(train)
X_test  <- build_design(test)

# Encode FTR as 0=H, 1=D, 2=A
ftr_to_int <- function(y) match(as.character(y), c("H","D","A")) - 1L
y_train <- ftr_to_int(train$FTR)
y_test  <- ftr_to_int(test$FTR)

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test,  label = y_test)

params <- list(
  objective        = "multi:softprob",
  num_class        = 3,
  eta              = 0.05,
  max_depth        = 5,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  eval_metric      = "mlogloss"
)

cat("Running 5-fold xgb.cv (early stopping patience = 30) ...\n")
set.seed(441)
cv_res <- xgb.cv(
  params = params,
  data   = dtrain,
  nrounds = 2000,
  nfold  = 5,
  early_stopping_rounds = 30,
  verbose = 0
)
# xgboost 3.x exposes best_iteration under $early_stop; earlier versions
# placed it at the top level. Handle both.
best_nrounds <- if (!is.null(cv_res$best_iteration)) {
  cv_res$best_iteration
} else {
  cv_res$early_stop$best_iteration
}
stopifnot(length(best_nrounds) == 1, best_nrounds > 0)
cat(sprintf("Best nrounds (CV) = %d (best mlogloss = %.5f).\n",
            best_nrounds, min(cv_res$evaluation_log$test_mlogloss_mean)))

set.seed(441)
xgb_fit <- xgb.train(
  params = params,
  data   = dtrain,
  nrounds = best_nrounds,
  verbose = 0
)
saveRDS(xgb_fit, file.path(model_dir, "xgb_fit.rds"))

# Predict ----------------------------------------------------------------
P_raw <- predict(xgb_fit, dtest)
# In xgboost 3.x multi:softprob returns an [n, 3] matrix directly; older
# versions returned a length-n*3 vector. Normalize either way.
if (is.matrix(P_raw)) {
  P <- P_raw
} else {
  P <- matrix(P_raw, ncol = 3, byrow = TRUE)
}
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

brier_xgb <- brier_mult(P, test$FTR)
ll_xgb    <- log_loss(P,   test$FTR)
auc_xgb   <- macro_auc(P,  test$FTR)

preds <- tibble(
  match_id = test$match_id,
  FTR      = test$FTR,
  Regime   = test$Regime,
  Date     = test$Date,
  Div      = test$Div,
  p_xgb_H = P[, "H"], p_xgb_D = P[, "D"], p_xgb_A = P[, "A"]
)
saveRDS(preds, file.path(data_dir, "xgb_preds_test.rds"))

# Importance -------------------------------------------------------------
imp <- xgb.importance(feature_names = colnames(X_train), model = xgb_fit) %>%
  as_tibble() %>%
  rename(feature = Feature)

write_csv(imp %>% mutate(across(where(is.numeric), ~ signif(., 4))),
          file.path(tbl_dir, "table_4_5_xgb_importance.csv"))
tex <- kable(imp %>% mutate(across(where(is.numeric), ~ signif(., 4))),
             format = "latex", booktabs = TRUE, longtable = TRUE,
             caption = sprintf("XGBoost feature importance (best nrounds = %d).",
                               best_nrounds),
             label = "tab:xgb-importance") %>%
  kable_styling(latex_options = c("repeat_header"), full_width = FALSE)
writeLines(as.character(tex), file.path(tbl_dir, "table_4_5_xgb_importance.tex"))

# Figure 4.2 -------------------------------------------------------------
top_imp <- imp %>% slice_head(n = 15)
p_imp <- ggplot(top_imp, aes(x = reorder(feature, Gain), y = Gain)) +
  geom_col(fill = "#b2182b") +
  coord_flip() +
  labs(x = NULL, y = "Gain",
       title = "Figure 4.2 - XGBoost: top 15 features by Gain",
       subtitle = sprintf("Best nrounds = %d; test Brier = %.4f", best_nrounds, brier_xgb)) +
  theme_eda
save_png(p_imp, "figure_4_2_xgb_importance", w = 8, h = 5)

cat("\n==== Section 4.4b findings ====\n")
cat(sprintf("1. Best nrounds = %d (CV).\n", best_nrounds))
cat(sprintf("2. Test Brier   = %.5f.\n", brier_xgb))
cat(sprintf("3. Test LogLoss = %.5f.\n", ll_xgb))
cat(sprintf("4. Test Macro-AUC = %.5f.\n", auc_xgb))
cat("5. Top 5 features by Gain:\n")
print(imp %>% slice_head(n = 5))
