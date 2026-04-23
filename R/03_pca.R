# 01_pca.R
# Section 3.2 - PCA on 9 market-shape features (train only).

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

train <- readRDS(file.path(data_dir, "features_train.rds"))

feat_cols <- c("p_fav","p_long","q_draw","entropy","log_ratio",
               "home_skew","overround","disagree_H","disagree_A")

X_train <- as.matrix(train[, feat_cols])

# -- Standardize (store center / scale) ------------------------------------
ctr <- colMeans(X_train)
scl <- apply(X_train, 2, sd)
Z_train <- sweep(sweep(X_train, 2, ctr, "-"), 2, scl, "/")

# -- Fit PCA on already-standardized data ---------------------------------
pca <- prcomp(Z_train, scale. = FALSE)

# Save fit + center + scale
saveRDS(list(pca = pca, center = ctr, scale = scl, feat_cols = feat_cols),
        file.path(data_dir, "pca_fit.rds"))

# -- Variance explained ----------------------------------------------------
var_exp <- pca$sdev^2 / sum(pca$sdev^2)
cum_exp <- cumsum(var_exp)

# -- Figure 3.1: scree + cumulative in a two-panel layout ------------------
var_df <- tibble(PC = factor(paste0("PC", seq_along(var_exp)),
                             levels = paste0("PC", seq_along(var_exp))),
                 var_exp = var_exp,
                 cum_exp = cum_exp)

p_scree <- ggplot(var_df, aes(x = PC, y = var_exp)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_line(aes(x = as.integer(PC), y = cum_exp), colour = "firebrick", linewidth = 0.8) +
  geom_point(aes(x = as.integer(PC), y = cum_exp), colour = "firebrick") +
  geom_hline(yintercept = 0.80, linetype = "dashed", colour = "grey40") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1.02)) +
  labs(x = "Principal component",
       y = "Variance explained (bar = per-PC, line = cumulative)",
       title = "Figure 3.1 - PCA scree plot (train only)",
       subtitle = "Dashed line: 80% cumulative-variance threshold") +
  theme_eda

save_png(p_scree, "figure_3_1_scree", w = 8, h = 5)

# -- Figure 3.2: biplot of PC1 vs PC2 colored by Tier ----------------------
scores <- as_tibble(pca$x[, 1:2]) %>%
  mutate(Tier = train$Tier)

loadings <- as_tibble(pca$rotation[, 1:2], rownames = "feature")

# Scale arrows by 3 * mean PC sd for visibility
arrow_scale <- 3 * mean(apply(pca$x[, 1:2], 2, sd))
loadings <- loadings %>%
  mutate(xend = PC1 * arrow_scale,
         yend = PC2 * arrow_scale)

p_biplot <- ggplot(scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = Tier), alpha = 0.25, size = 0.5) +
  scale_colour_manual(values = c("Top" = "#2c7fb8", "Second" = "#fdae61")) +
  geom_segment(data = loadings,
               aes(x = 0, y = 0, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.15, "cm")),
               colour = "black", linewidth = 0.5) +
  geom_text(data = loadings,
            aes(x = xend * 1.1, y = yend * 1.1, label = feature),
            colour = "black", size = 3.2, fontface = "bold") +
  labs(x = sprintf("PC1 (%.1f%% var)", var_exp[1] * 100),
       y = sprintf("PC2 (%.1f%% var)", var_exp[2] * 100),
       title = "Figure 3.2 - PCA biplot (PC1 vs PC2, train)",
       colour = "Tier") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  theme_eda

save_png(p_biplot, "figure_3_2_pca_biplot", w = 8, h = 5)

# -- Table 3.1: loadings for first 4 PCs ----------------------------------
load_tbl <- as_tibble(pca$rotation[, 1:4], rownames = "feature") %>%
  mutate(across(PC1:PC4, ~ signif(., 3)))

cum_row <- tibble(
  feature = "cumulative_var_pct",
  PC1 = signif(cum_exp[1] * 100, 3),
  PC2 = signif(cum_exp[2] * 100, 3),
  PC3 = signif(cum_exp[3] * 100, 3),
  PC4 = signif(cum_exp[4] * 100, 3)
)

load_full <- bind_rows(load_tbl, cum_row)
write_csv(load_full, file.path(tbl_dir, "table_3_1_pca_loadings.csv"))

tex <- kable(load_full, format = "latex", booktabs = TRUE,
             caption = "PCA loadings for the first four principal components; final row reports cumulative variance (%) .",
             label = "tab:pca-loadings") %>%
  kable_styling(latex_options = c("hold_position"))
writeLines(as.character(tex), file.path(tbl_dir, "table_3_1_pca_loadings.tex"))

# -- Top drivers per PC ----------------------------------------------------
top_drivers <- function(load_vec, feats, n = 3) {
  o <- order(abs(load_vec), decreasing = TRUE)[1:n]
  paste0(feats[o], " (", sprintf("%+.2f", load_vec[o]), ")", collapse = ", ")
}

cat("==== Section 3.2 findings ====\n")
cat(sprintf("1. Variance explained: PC1 = %.1f%%, PC2 = %.1f%%, PC3 = %.1f%%, PC4 = %.1f%%.\n",
            var_exp[1] * 100, var_exp[2] * 100, var_exp[3] * 100, var_exp[4] * 100))
cat(sprintf("2. Cumulative variance: PC1-2 = %.1f%%, PC1-3 = %.1f%%, PC1-4 = %.1f%%.\n",
            cum_exp[2] * 100, cum_exp[3] * 100, cum_exp[4] * 100))
cat(sprintf("3. PC1 top 3 drivers: %s.\n", top_drivers(pca$rotation[, 1], feat_cols)))
cat(sprintf("4. PC2 top 3 drivers: %s.\n", top_drivers(pca$rotation[, 2], feat_cols)))
cat(sprintf("5. PC3 top 3 drivers: %s.\n", top_drivers(pca$rotation[, 3], feat_cols)))

k_pc <- which(cum_exp >= 0.80)[1]
cat(sprintf("6. Smallest k such that cumulative variance >= 80%%: k_pc = %d (cum = %.1f%%).\n",
            k_pc, cum_exp[k_pc] * 100))
