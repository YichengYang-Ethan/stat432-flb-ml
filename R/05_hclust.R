# 03_hclust.R
# Section 3.4 - Ward-linkage hierarchical clustering for robustness check.
# dist() on ~32K points needs ~2.5 GB RAM; fall back to a K-Means-stratified
# 10,000-row subsample if dist() errors.

set.seed(441)
suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
  library(mclust)
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

train   <- readRDS(file.path(data_dir, "train_with_regime.rds"))
pca_fit <- readRDS(file.path(data_dir, "pca_fit.rds"))
km_fit  <- readRDS(file.path(data_dir, "kmeans_fit.rds"))

k_pc   <- km_fit$k_pc
K_star <- km_fit$K_star
pca    <- pca_fit$pca

Z_train <- pca$x[, 1:k_pc, drop = FALSE]
stopifnot(nrow(Z_train) == nrow(train))

# -- Attempt full-train hclust; fall back to stratified 10K subsample ------
use_subsample <- TRUE
d <- tryCatch({
  cat("Attempting dist() on full training set (", nrow(Z_train), " rows)...\n", sep = "")
  res <- dist(Z_train)
  use_subsample <- FALSE
  res
}, error = function(e) {
  cat("dist() on full training set failed: ", conditionMessage(e), "\n", sep = "")
  NULL
})

if (use_subsample) {
  cat("Falling back to 10,000-row K-Means-stratified subsample.\n")
  target_n <- 10000
  sub_idx <- train %>%
    mutate(row_id = row_number()) %>%
    group_by(cluster) %>%
    mutate(take = pmin(n(), round(target_n * n() / nrow(train)))) %>%
    group_map(~ sample(.x$row_id, size = first(.x$take)), .keep = TRUE) %>%
    unlist() %>%
    sort()
  cat(sprintf("Subsample rows: %d (stratified by K-Means cluster).\n", length(sub_idx)))
  Z_sub <- Z_train[sub_idx, , drop = FALSE]
  km_sub_labels <- train$cluster[sub_idx]
  d <- dist(Z_sub)
} else {
  cat("Using full training set for hclust.\n")
  Z_sub <- Z_train
  km_sub_labels <- train$cluster
}

# -- Ward linkage ----------------------------------------------------------
hc <- hclust(d, method = "ward.D2")
hc_labels <- factor(cutree(hc, k = K_star))

# -- Figure 3.5: dendrogram, top 30 merges, with K* cut line -------------
n_obs <- length(hc$order)
top_heights <- tail(sort(hc$height), 30)
cut_height  <- mean(sort(hc$height, decreasing = TRUE)[c(K_star - 1, K_star)])

# Use base graphics (ggplot for dendrograms is awkward with large n)
png(file.path(fig_dir, "figure_3_5_dendrogram.png"),
    width = 8, height = 5, units = "in", res = 300)
op <- par(mar = c(2, 4, 4, 1))
plot(hc,
     labels = FALSE,
     hang   = -1,
     main   = sprintf("Figure 3.5 - Ward dendrogram (top of tree; cut at K = %d)", K_star),
     sub    = ifelse(use_subsample,
                     sprintf("Based on stratified subsample of %s training matches",
                             format(nrow(Z_sub), big.mark = ",")),
                     sprintf("Based on full training set (%s matches)",
                             format(nrow(Z_sub), big.mark = ","))),
     xlab   = "",
     ylab   = "Ward linkage height",
     ylim   = c(min(top_heights) * 0.9, max(hc$height) * 1.05))
abline(h = cut_height, col = "firebrick", lty = 2, lwd = 1.5)
par(op)
dev.off()

# -- Table 3.3: cross-tab K-Means vs Hclust ------------------------------
ari <- adjustedRandIndex(as.integer(km_sub_labels), as.integer(hc_labels))

xtab <- table(KMeans = km_sub_labels, Hclust = hc_labels)
xtab_df <- as.data.frame.matrix(xtab) %>%
  rownames_to_column("KMeans_cluster")

write_csv(xtab_df, file.path(tbl_dir, "table_3_3_kmeans_vs_hclust.csv"))

tex <- kable(xtab_df, format = "latex", booktabs = TRUE,
             caption = sprintf("Cross-tabulation of K-Means and Ward hierarchical clusters at K = %d. Adjusted Rand Index = %.3f.%s",
                               K_star, ari,
                               ifelse(use_subsample,
                                      sprintf(" Computed on a stratified subsample of %s training matches.",
                                              format(nrow(Z_sub), big.mark = ",")), "")),
             label = "tab:kmeans-vs-hclust") %>%
  kable_styling(latex_options = c("hold_position"))
writeLines(as.character(tex), file.path(tbl_dir, "table_3_3_kmeans_vs_hclust.tex"))

# -- Save hclust artifacts so downstream scripts can reuse ---------------
saveRDS(list(hc = hc, labels = hc_labels, use_subsample = use_subsample,
             sub_idx = if (use_subsample) sub_idx else NULL,
             ari = ari),
        file.path(data_dir, "hclust_fit.rds"))

cat("\n==== Section 3.4 findings ====\n")
cat(sprintf("1. Hierarchical clustering fit via %s.\n",
            ifelse(use_subsample,
                   sprintf("stratified subsample of %s rows", format(nrow(Z_sub), big.mark = ",")),
                   sprintf("full training set of %s rows",  format(nrow(Z_sub), big.mark = ",")))))
cat(sprintf("2. Ward-linkage cut at K = %d yields cluster sizes: %s.\n",
            K_star, paste(as.vector(table(hc_labels)), collapse = ", ")))
cat(sprintf("3. Adjusted Rand Index between K-Means and Hclust: %.3f.\n", ari))
cat(sprintf("4. Interpretation: %s.\n",
            ifelse(ari > 0.7,
                   "regime structure is robust (ARI > 0.7)",
                   ifelse(ari > 0.5,
                          "regime structure is moderately robust (0.5 < ARI <= 0.7)",
                          "regime structure is only weakly robust (ARI <= 0.5)"))))
cat("5. Cross-tab K-Means vs Hclust:\n")
print(xtab)
