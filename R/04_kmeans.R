# 02_kmeans.R
# Section 3.3 - K-Means clustering on train-fitted PCA scores.

set.seed(441)
suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(kableExtra)
  library(cluster)
  library(fpc)
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

train <- readRDS(file.path(data_dir, "features_train.rds"))
pca_fit <- readRDS(file.path(data_dir, "pca_fit.rds"))

feat_cols <- pca_fit$feat_cols
pca       <- pca_fit$pca
var_exp   <- pca$sdev^2 / sum(pca$sdev^2)
cum_exp   <- cumsum(var_exp)
k_pc      <- which(cum_exp >= 0.80)[1]
cat(sprintf("Retained k_pc = %d PCs (cumulative variance = %.1f%%).\n",
            k_pc, cum_exp[k_pc] * 100))

# Projected training scores on the retained PCs
Z_train <- pca$x[, 1:k_pc, drop = FALSE]

# -- Sweep K = 2..10, record WSS / CH / Silhouette ------------------------
# Silhouette uses a 5000-row subsample (for runtime at ~32K).
sub_n <- 5000
sub_idx <- sample.int(nrow(Z_train), sub_n)

Ks <- 2:10
res <- map_dfr(Ks, function(K) {
  km <- kmeans(Z_train, centers = K, nstart = 25, iter.max = 50)
  # Calinski-Harabasz
  ch <- calinhara(Z_train, km$cluster, cn = K)
  # Silhouette on the subsample
  d_sub <- dist(Z_train[sub_idx, ])
  sil <- silhouette(km$cluster[sub_idx], d_sub)
  sil_mean <- mean(sil[, "sil_width"])
  tibble(K = K, WSS = km$tot.withinss, CH = ch, Silhouette = sil_mean)
})

# Choose K* = argmax Silhouette over K in 3..6
choice_window <- res %>% filter(K %in% 3:6)
K_star <- choice_window$K[which.max(choice_window$Silhouette)]
cat(sprintf("K* (argmax silhouette over K in 3..6) = %d.\n", K_star))

cat("\nCluster-number criteria:\n")
print(res %>% mutate(across(c(WSS, CH, Silhouette), ~ signif(., 4))))

# -- Figure 3.3: three-panel selection plot -------------------------------
p_wss <- ggplot(res, aes(x = K, y = WSS)) +
  geom_line() + geom_point() +
  geom_vline(xintercept = K_star, linetype = "dashed", colour = "firebrick") +
  scale_x_continuous(breaks = Ks) +
  labs(title = "(a) WSS (elbow)", x = "K", y = "Within-cluster SS") +
  theme_eda

p_ch <- ggplot(res, aes(x = K, y = CH)) +
  geom_line() + geom_point() +
  geom_vline(xintercept = K_star, linetype = "dashed", colour = "firebrick") +
  scale_x_continuous(breaks = Ks) +
  labs(title = "(b) Calinski-Harabasz", x = "K", y = "CH index") +
  theme_eda

p_sil <- ggplot(res, aes(x = K, y = Silhouette)) +
  geom_line() + geom_point() +
  geom_vline(xintercept = K_star, linetype = "dashed", colour = "firebrick") +
  scale_x_continuous(breaks = Ks) +
  labs(title = "(c) Silhouette", x = "K", y = "Mean silhouette width") +
  theme_eda

p_selection <- p_wss + p_ch + p_sil +
  plot_annotation(title = sprintf("Figure 3.3 - K-Means selection criteria (dashed line at K* = %d)", K_star))
save_png(p_selection, "figure_3_3_k_selection", w = 11, h = 4.5)

# -- Refit at K* on full training set -------------------------------------
set.seed(441)
km_final <- kmeans(Z_train, centers = K_star, nstart = 50, iter.max = 100)

saveRDS(list(km = km_final, k_pc = k_pc, K_star = K_star),
        file.path(data_dir, "kmeans_fit.rds"))

train$cluster <- factor(km_final$cluster)
saveRDS(train, file.path(data_dir, "train_with_regime.rds"))

# -- Figure 3.4: PC1 vs PC2 colored by cluster, with centroids ------------
scores_df <- as_tibble(pca$x[, 1:2]) %>%
  mutate(cluster = train$cluster)
centroids_pc12 <- as_tibble(km_final$centers[, 1:2]) %>%
  mutate(cluster = factor(seq_len(K_star)))

p_clusters <- ggplot(scores_df, aes(x = PC1, y = PC2, colour = cluster)) +
  geom_point(alpha = 0.35, size = 0.5) +
  geom_point(data = centroids_pc12,
             aes(x = PC1, y = PC2, fill = cluster),
             shape = 4, size = 6, stroke = 1.5, colour = "black") +
  labs(x = sprintf("PC1 (%.1f%% var)", var_exp[1] * 100),
       y = sprintf("PC2 (%.1f%% var)", var_exp[2] * 100),
       title = sprintf("Figure 3.4 - K-Means regimes on PC1-PC2 (K* = %d)", K_star),
       subtitle = "Black X = cluster centroid") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2)),
         fill = "none") +
  theme_eda

save_png(p_clusters, "figure_3_4_kmeans_on_pc12", w = 8, h = 5)

# -- Table 3.2: per-cluster profile ---------------------------------------
top_tier_frac <- train %>%
  group_by(cluster) %>%
  summarise(top_tier_share = mean(Tier == "Top"), .groups = "drop")

div_shares <- train %>%
  count(cluster, Div) %>%
  group_by(cluster) %>%
  arrange(desc(n)) %>%
  slice_head(n = 3) %>%
  summarise(top3_divs = paste(sprintf("%s(%.0f%%)", Div, n / sum(n) * 100),
                              collapse = ", "),
            .groups = "drop")

# Approximate shares correctly: share of matches *in that cluster* that come from each Div
div_shares <- train %>%
  group_by(cluster) %>%
  mutate(cluster_n = n()) %>%
  group_by(cluster, Div) %>%
  summarise(div_n = n(), cluster_n = first(cluster_n), .groups = "drop") %>%
  mutate(share = div_n / cluster_n) %>%
  group_by(cluster) %>%
  arrange(desc(share)) %>%
  slice_head(n = 3) %>%
  summarise(top3_divs = paste(sprintf("%s(%.0f%%)", Div, share * 100),
                              collapse = ", "),
            .groups = "drop")

feat_profile <- train %>%
  group_by(cluster) %>%
  summarise(across(all_of(feat_cols), mean), n = n(), .groups = "drop") %>%
  mutate(n_pct = n / sum(n) * 100)

profile <- feat_profile %>%
  left_join(top_tier_frac, by = "cluster") %>%
  left_join(div_shares,    by = "cluster") %>%
  relocate(cluster, n, n_pct, all_of(feat_cols),
           top_tier_share, top3_divs)

write_csv(profile %>% mutate(across(where(is.numeric), ~ signif(., 3))),
          file.path(tbl_dir, "table_3_2_cluster_profiles.csv"))

tex <- kable(profile %>% mutate(across(where(is.numeric), ~ signif(., 3))),
             format = "latex", booktabs = TRUE, longtable = TRUE,
             caption = sprintf("K-Means cluster profiles at K* = %d.", K_star),
             label = "tab:cluster-profiles") %>%
  kable_styling(latex_options = c("repeat_header", "scale_down"),
                full_width = FALSE)
writeLines(as.character(tex), file.path(tbl_dir, "table_3_2_cluster_profiles.tex"))

# -- One-line interpretations --------------------------------------------
global_mean <- train %>% summarise(across(all_of(feat_cols), mean))
interpret <- profile %>%
  rowwise() %>%
  mutate(
    blurb = {
      diffs <- c_across(all_of(feat_cols)) - as.numeric(global_mean[1, feat_cols])
      rel   <- diffs / as.numeric(global_mean[1, feat_cols])
      top3  <- order(abs(rel), decreasing = TRUE)[1:3]
      paste(sprintf("%s %s (%+.0f%%)",
                    feat_cols[top3],
                    ifelse(diffs[top3] > 0, "up", "down"),
                    rel[top3] * 100),
            collapse = "; ")
    }
  ) %>%
  ungroup() %>%
  select(cluster, n, n_pct, top_tier_share, blurb)

cat("\n==== Section 3.3 findings ====\n")
cat(sprintf("1. K* = %d (chosen to maximize silhouette over K in 3..6).\n", K_star))
cat(sprintf("2. k_pc = %d retained PCs (cumulative variance %.1f%%).\n", k_pc, cum_exp[k_pc] * 100))
cat("3. Cluster sizes and Top-tier share:\n")
print(interpret %>% mutate(across(c(n_pct, top_tier_share), ~ signif(., 3))))
cat("4. One-line interpretation of each cluster (top 3 distinctive features vs global mean):\n")
for (i in seq_len(nrow(interpret))) {
  cat(sprintf("   cluster %s: %s\n", interpret$cluster[i], interpret$blurb[i]))
}
cat(sprintf("5. WSS at K*: %s; CH at K*: %s; silhouette at K*: %s.\n",
            format(signif(res$WSS[res$K == K_star], 4), big.mark = ","),
            signif(res$CH[res$K == K_star], 4),
            signif(res$Silhouette[res$K == K_star], 3)))
