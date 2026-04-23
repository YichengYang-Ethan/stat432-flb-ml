# 03_elasticnet.R
# Section 4.3 - Regularized multinomial logit via glmnet.

set.seed(441)
suppressPackageStartupMessages({
  library(tidyverse)
  library(glmnet)
  library(kableExtra)
  library(pROC)
})

root      <- "~/Desktop/FLB_Final_Code"
data_dir  <- file.path(root, "data")
tbl_dir   <- file.path(root, "tables")
model_dir <- file.path(root, "models")

train <- readRDS(file.path(data_dir, "train_features.rds"))
test  <- readRDS(file.path(data_dir, "test_features.rds"))

scaling <- readRDS(file.path(data_dir, "feature_scaling.rds"))
cont_feats <- scaling$cont_feats

# Build X, y. Features = 37 standardized continuous + Regime one-hot (3 dummies).
build_design <- function(df) {
  X_cont <- as.matrix(df[, cont_feats])
  R <- model.matrix(~ Regime - 1, data = df)[, -1, drop = FALSE]
  colnames(R) <- paste0("Regime_", sub("^Regime", "", colnames(R)))
  cbind(X_cont, R)
}

X_train <- build_design(train)
X_test  <- build_design(test)
y_train <- train$FTR
y_test  <- test$FTR
cat("Design matrix columns:", ncol(X_train), "\n")

# -- Fit ------------------------------------------------------------------
set.seed(441)
cv_fit <- cv.glmnet(
  x = X_train, y = y_train,
  family = "multinomial",
  type.multinomial = "grouped",
  alpha = 0.5,
  nfolds = 10,
  type.measure = "deviance"
)
lambda_min <- cv_fit$lambda.min
cat(sprintf("Chosen lambda.min = %.6f (log = %.3f).\n",
            lambda_min, log(lambda_min)))

saveRDS(cv_fit, file.path(model_dir, "elasticnet_fit.rds"))

# -- Predict --------------------------------------------------------------
P_raw <- predict(cv_fit, newx = X_test, s = "lambda.min", type = "response")
# P_raw comes back as a 3D array [n, 3, 1]; drop the extra dim.
P <- P_raw[, , 1]
# Ensure column order = H, D, A
P <- P[, c("H","D","A"), drop = FALSE]

# -- Metrics --------------------------------------------------------------
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

brier_en <- brier_mult(P, y_test)
ll_en    <- log_loss(P,   y_test)
auc_en   <- macro_auc(P,  y_test)

# -- Save predictions -----------------------------------------------------
preds <- tibble(
  match_id = test$match_id,
  FTR      = y_test,
  Regime   = test$Regime,
  Date     = test$Date,
  Div      = test$Div,
  p_en_H = P[, "H"], p_en_D = P[, "D"], p_en_A = P[, "A"]
)
saveRDS(preds, file.path(data_dir, "elasticnet_preds_test.rds"))

# -- Coefficients ---------------------------------------------------------
coefs_raw <- coef(cv_fit, s = "lambda.min")  # list with H, D, A
build_coef_tbl <- function() {
  feats <- rownames(coefs_raw$H)
  tibble(
    feature = feats,
    H = as.numeric(coefs_raw$H),
    D = as.numeric(coefs_raw$D),
    A = as.numeric(coefs_raw$A)
  )
}
coef_tbl <- build_coef_tbl()
n_nz <- coef_tbl %>%
  summarise(across(c(H, D, A), ~ sum(abs(.) > 1e-10)))

write_csv(coef_tbl %>% mutate(across(c(H, D, A), ~ signif(., 4))),
          file.path(tbl_dir, "table_4_3_elasticnet_coefs.csv"))

tex <- kable(coef_tbl %>% mutate(across(c(H, D, A), ~ signif(., 4))),
             format = "latex", booktabs = TRUE, longtable = TRUE,
             caption = sprintf("Elastic-Net (alpha = 0.5) coefficients at lambda.min = %.5f.",
                               lambda_min),
             label = "tab:elasticnet-coefs") %>%
  kable_styling(latex_options = c("repeat_header"), full_width = FALSE)
writeLines(as.character(tex), file.path(tbl_dir, "table_4_3_elasticnet_coefs.tex"))

cat("\n==== Section 4.3 findings ====\n")
cat(sprintf("1. Chosen lambda.min = %.6f (log = %.3f).\n", lambda_min, log(lambda_min)))
cat(sprintf("2. Non-zero coefficients per class: H = %d, D = %d, A = %d (grouped penalty ties them).\n",
            n_nz$H, n_nz$D, n_nz$A))
cat(sprintf("3. Test Brier   = %.5f.\n", brier_en))
cat(sprintf("4. Test LogLoss = %.5f.\n", ll_en))
cat(sprintf("5. Test Macro-AUC = %.5f.\n", auc_en))
