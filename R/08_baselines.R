# 02_baselines.R
# Section 4.2 - naive (de-vigged) and Shin-adjusted baseline probabilities.

set.seed(441)
suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
  library(pROC)
})

root     <- "~/Desktop/FLB_Final_Code"
data_dir <- file.path(root, "data")
tbl_dir  <- file.path(root, "tables")

train <- readRDS(file.path(data_dir, "train_features.rds"))
test  <- readRDS(file.path(data_dir, "test_features.rds"))

books <- list(
  B365 = c("B365H","B365D","B365A"),
  BW   = c("BWH","BWD","BWA"),
  IW   = c("IWH","IWD","IWA"),
  WH   = c("WHH","WHD","WHA"),
  VC   = c("VCH","VCD","VCA"),
  PSC  = c("PSCH","PSCD","PSCA")
)

# -- Shin de-vigging (per bookmaker per match) ---------------------------
# Solve Sum_k p_k(z) = 1 where
#   p_k(z) = (sqrt(z^2 + 4(1-z) * pi_k^2 / B) - z) / (2(1-z))
# for z in (0, 0.5). Use uniroot.
shin_probs <- function(oH, oD, oA) {
  piH <- 1 / oH; piD <- 1 / oD; piA <- 1 / oA
  B   <- piH + piD + piA
  p_fun <- function(z) {
    pH <- (sqrt(z^2 + 4 * (1 - z) * piH^2 / B) - z) / (2 * (1 - z))
    pD <- (sqrt(z^2 + 4 * (1 - z) * piD^2 / B) - z) / (2 * (1 - z))
    pA <- (sqrt(z^2 + 4 * (1 - z) * piA^2 / B) - z) / (2 * (1 - z))
    list(sum = pH + pD + pA, p = c(H = pH, D = pD, A = pA))
  }
  f <- function(z) p_fun(z)$sum - 1
  # f(0) = B - 1 > 0 (overround positive); f(0.5) should be < 0 except in
  # pathological cases. Guard with tryCatch.
  out <- tryCatch({
    if (f(1e-6) <= 0) {
      # Overround <= 1; fall back to naive de-vigging.
      z <- 0
      p_naive <- c(H = piH / B, D = piD / B, A = piA / B)
      return(p_naive)
    }
    uroot <- uniroot(f, lower = 1e-6, upper = 0.5, tol = 1e-10)
    z <- uroot$root
    p_fun(z)$p
  }, error = function(e) {
    # Fall back to naive on failure
    c(H = piH / B, D = piD / B, A = piA / B)
  })
  out
}

# Vectorize Shin over the six books, then average
shin_avg <- function(row_odds) {
  # row_odds is a named numeric vector with columns from `books`
  mat <- matrix(0, nrow = length(books), ncol = 3,
                dimnames = list(names(books), c("H","D","A")))
  for (i in seq_along(books)) {
    cols <- books[[i]]
    mat[i, ] <- shin_probs(row_odds[[cols[1]]],
                           row_odds[[cols[2]]],
                           row_odds[[cols[3]]])
  }
  colMeans(mat)
}

compute_shin_for_df <- function(df) {
  n <- nrow(df)
  out <- matrix(0, nrow = n, ncol = 3, dimnames = list(NULL, c("H","D","A")))
  odds_cols <- unlist(books)
  odds_mat  <- as.matrix(df[, odds_cols])
  for (i in seq_len(n)) {
    row <- setNames(as.list(odds_mat[i, ]), odds_cols)
    out[i, ] <- shin_avg(row)
  }
  as_tibble(out)
}

cat("Computing Shin probabilities on train (", nrow(train), ") and test (",
    nrow(test), ") ...\n", sep = "")
shin_train <- compute_shin_for_df(train)
shin_test  <- compute_shin_for_df(test)

# -- Naive probabilities (already computed in Section 2) ----------------
train <- train %>%
  mutate(p_naive_H = q_H_mean,
         p_naive_D = q_D_mean,
         p_naive_A = q_A_mean,
         p_shin_H  = shin_train$H,
         p_shin_D  = shin_train$D,
         p_shin_A  = shin_train$A)
test  <- test %>%
  mutate(p_naive_H = q_H_mean,
         p_naive_D = q_D_mean,
         p_naive_A = q_A_mean,
         p_shin_H  = shin_test$H,
         p_shin_D  = shin_test$D,
         p_shin_A  = shin_test$A)

# Sanity checks on Shin probs
stopifnot(all(abs(rowSums(shin_test) - 1) < 1e-6))
stopifnot(all(shin_test > 0 & shin_test < 1))

# -- Metric helpers ------------------------------------------------------
log_loss <- function(P, y) {
  # P has rows summing to 1, columns = levels(y). y factor with same levels.
  levs <- colnames(P)
  y_int <- match(as.character(y), levs)
  p_true <- P[cbind(seq_len(nrow(P)), y_int)]
  p_true <- pmax(p_true, 1e-15)
  -mean(log(p_true))
}

brier_mult <- function(P, y) {
  levs <- colnames(P)
  Y <- outer(as.character(y), levs, `==`) * 1L
  mean(rowSums((P - Y)^2))
}

macro_auc <- function(P, y) {
  levs <- colnames(P)
  aucs <- sapply(levs, function(k) {
    y_bin <- as.integer(as.character(y) == k)
    if (length(unique(y_bin)) < 2) return(NA_real_)
    roc_k <- roc(y_bin, P[, k], quiet = TRUE)
    as.numeric(auc(roc_k))
  })
  mean(aucs, na.rm = TRUE)
}

# -- Evaluate on test ---------------------------------------------------
P_naive <- as.matrix(test[, c("p_naive_H","p_naive_D","p_naive_A")])
colnames(P_naive) <- c("H","D","A")
P_shin  <- as.matrix(test[, c("p_shin_H","p_shin_D","p_shin_A")])
colnames(P_shin)  <- c("H","D","A")

y_test <- test$FTR

res <- tibble(
  Model    = c("Naive (de-vigged)", "Shin (1993)"),
  Brier    = c(brier_mult(P_naive, y_test), brier_mult(P_shin, y_test)),
  LogLoss  = c(log_loss(P_naive,  y_test), log_loss(P_shin,  y_test)),
  MacroAUC = c(macro_auc(P_naive, y_test), macro_auc(P_shin, y_test))
)

# -- Save predictions ---------------------------------------------------
preds <- tibble(
  match_id = test$match_id,
  FTR      = y_test,
  Regime   = test$Regime,
  Date     = test$Date,
  Div      = test$Div,
  p_naive_H = P_naive[, "H"], p_naive_D = P_naive[, "D"], p_naive_A = P_naive[, "A"],
  p_shin_H  = P_shin[,  "H"], p_shin_D  = P_shin[,  "D"], p_shin_A  = P_shin[,  "A"]
)
saveRDS(preds, file.path(data_dir, "baseline_preds_test.rds"))

write_csv(res %>% mutate(across(where(is.numeric), ~ signif(., 4))),
          file.path(tbl_dir, "table_4_2_baselines.csv"))
tex <- kable(res %>% mutate(across(where(is.numeric), ~ signif(., 4))),
             format = "latex", booktabs = TRUE,
             caption = "Baseline probabilities evaluated on the test set.",
             label = "tab:baselines") %>%
  kable_styling(latex_options = c("hold_position"))
writeLines(as.character(tex), file.path(tbl_dir, "table_4_2_baselines.tex"))

cat("\n==== Section 4.2 findings ====\n")
cat("1. Baseline performance on test:\n")
print(res %>% mutate(across(where(is.numeric), ~ signif(., 4))))
better <- if (res$Brier[2] < res$Brier[1]) "Shin" else "Naive"
cat(sprintf("2. Lower Brier baseline: %s.\n", better))
cat(sprintf("3. Brier delta (Naive - Shin): %+.5f.\n", res$Brier[1] - res$Brier[2]))
cat(sprintf("4. LogLoss delta (Naive - Shin): %+.5f.\n", res$LogLoss[1] - res$LogLoss[2]))
cat(sprintf("5. Macro-AUC delta (Shin - Naive): %+.5f.\n", res$MacroAUC[2] - res$MacroAUC[1]))
