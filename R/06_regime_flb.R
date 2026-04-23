# 04_regime_assignment.R
# Section 3.5 - assign test matches to train-fitted regimes,
# then build regime-specific reliability diagrams.

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

pca_fit <- readRDS(file.path(data_dir, "pca_fit.rds"))
km_fit  <- readRDS(file.path(data_dir, "kmeans_fit.rds"))
train   <- readRDS(file.path(data_dir, "train_with_regime.rds"))
test    <- readRDS(file.path(data_dir, "features_test.rds"))

feat_cols <- pca_fit$feat_cols
ctr       <- pca_fit$center
scl       <- pca_fit$scale
pca       <- pca_fit$pca
k_pc      <- km_fit$k_pc
K_star    <- km_fit$K_star
centroids <- km_fit$km$centers

# -- Step 1: assign test matches to nearest train-fitted centroid ----------
X_test <- as.matrix(test[, feat_cols])
Z_test <- sweep(sweep(X_test, 2, ctr, "-"), 2, scl, "/")
P_test <- Z_test %*% pca$rotation[, 1:k_pc, drop = FALSE]

# Nearest centroid (Euclidean in PC space)
dist_to_centroids <- function(X, C) {
  # rows of X, centroids in rows of C
  sq_X <- rowSums(X^2)
  sq_C <- rowSums(C^2)
  # (x-c)^2 = x^2 + c^2 - 2 x.c
  cross <- X %*% t(C)
  sweep(sweep(-2 * cross, 1, sq_X, "+"), 2, sq_C, "+")
}

d_mat <- dist_to_centroids(P_test, centroids)
test_cluster <- apply(d_mat, 1, which.min)
test$cluster <- factor(test_cluster, levels = seq_len(K_star))
saveRDS(test, file.path(data_dir, "test_with_regime.rds"))

cat(sprintf("Assigned %s test matches to K* = %d regimes.\n",
            format(nrow(test), big.mark = ","), K_star))
cat("Train cluster sizes: "); print(table(train$cluster))
cat("Test  cluster sizes: "); print(table(test$cluster))

# -- Step 2: regime-specific reliability diagrams (train + test pooled) ----
all_dat <- bind_rows(
  train %>% mutate(split = "train"),
  test  %>% mutate(split = "test")
)

long <- bind_rows(
  all_dat %>% transmute(p = q_H_mean, y = as.integer(FTR == "H"),
                        cluster, split),
  all_dat %>% transmute(p = q_D_mean, y = as.integer(FTR == "D"),
                        cluster, split),
  all_dat %>% transmute(p = q_A_mean, y = as.integer(FTR == "A"),
                        cluster, split)
)

n_bins <- 20
breaks <- seq(0, 1, length.out = n_bins + 1)

wilson_ci <- function(k, n, z = 1.96) {
  if (n == 0) return(c(NA_real_, NA_real_))
  phat   <- k / n
  denom  <- 1 + z^2 / n
  centre <- (phat + z^2 / (2 * n)) / denom
  halfw  <- (z * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2))) / denom
  c(centre - halfw, centre + halfw)
}

bin_tbl <- long %>%
  mutate(bin = cut(p, breaks = breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(cluster, bin) %>%
  summarise(
    mean_p   = mean(p),
    realized = mean(y),
    k        = sum(y),
    n        = n(),
    .groups  = "drop"
  ) %>%
  filter(n > 0) %>%
  rowwise() %>%
  mutate(ci_lo = wilson_ci(k, n)[1],
         ci_hi = wilson_ci(k, n)[2]) %>%
  ungroup()

# Per-regime OLS slope with 95% CI (weighted by n)
slope_tbl <- bin_tbl %>%
  group_by(cluster) %>%
  group_modify(~ {
    fit  <- lm(realized ~ mean_p, data = .x, weights = .x$n)
    b    <- coef(fit)
    ci   <- confint(fit, "mean_p", level = 0.95)
    tibble(slope = unname(b[2]),
           intercept = unname(b[1]),
           slope_lo = ci[1, 1],
           slope_hi = ci[1, 2],
           n_pairs = nrow(.x))
  }) %>%
  ungroup()

cat("\nRegime calibration slopes (95% CI):\n")
print(slope_tbl %>% mutate(across(where(is.numeric), ~ signif(., 3))))

# -- Figure 3.6: faceted reliability diagrams ----------------------------
label_df <- slope_tbl %>%
  mutate(label = sprintf("regime %s: slope = %.3f (%.3f, %.3f)",
                         cluster, slope, slope_lo, slope_hi))
label_lookup <- setNames(label_df$label, as.character(label_df$cluster))

p_facet <- ggplot(bin_tbl, aes(x = mean_p, y = realized)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                width = 0.02, colour = "grey30", alpha = 0.6) +
  geom_point(aes(size = n), colour = "firebrick", alpha = 0.85) +
  scale_size_continuous(name = "Bin count", range = c(1, 4)) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  facet_wrap(vars(cluster), labeller = as_labeller(label_lookup)) +
  labs(x = "Implied probability (de-vigged)",
       y = "Realized frequency",
       title = sprintf("Figure 3.6 - Reliability diagram per regime (K* = %d)", K_star),
       subtitle = "Train + test pooled; Wilson 95% CI; slope from per-bin OLS weighted by n") +
  theme_eda

save_png(p_facet, "figure_3_6_reliability_by_regime", w = 10, h = 7)

# -- Table 3.4: regime summary ----------------------------------------------
regime_train_n <- train %>% count(cluster) %>% rename(n_train = n)
regime_test_n  <- test  %>% count(cluster) %>% rename(n_test = n)

regime_feat <- all_dat %>%
  group_by(cluster) %>%
  summarise(
    mean_p_fav      = mean(p_fav),
    mean_overround  = mean(overround),
    mean_disagree   = mean(disagree_H + disagree_A),
    .groups = "drop"
  )

div_top3 <- all_dat %>%
  group_by(cluster) %>%
  mutate(cluster_n = n()) %>%
  group_by(cluster, Div) %>%
  summarise(share = n() / first(cluster_n), .groups = "drop") %>%
  group_by(cluster) %>%
  arrange(desc(share)) %>%
  slice_head(n = 3) %>%
  summarise(dominant_div_top3 = paste(sprintf("%s(%.0f%%)", Div, share * 100),
                                      collapse = ", "),
            .groups = "drop")

regime_summary <- regime_feat %>%
  left_join(regime_train_n, by = "cluster") %>%
  left_join(regime_test_n,  by = "cluster") %>%
  left_join(slope_tbl %>% select(cluster, slope, slope_lo, slope_hi),
            by = "cluster") %>%
  left_join(div_top3, by = "cluster") %>%
  mutate(regime_name_placeholder = sprintf("Regime %s", cluster)) %>%
  relocate(cluster, regime_name_placeholder, n_train, n_test,
           mean_p_fav, mean_overround, mean_disagree,
           slope, slope_lo, slope_hi, dominant_div_top3)

write_csv(regime_summary %>% mutate(across(where(is.numeric), ~ signif(., 3))),
          file.path(tbl_dir, "table_3_4_regime_summary.csv"))

tex <- kable(regime_summary %>% mutate(across(where(is.numeric), ~ signif(., 3))),
             format = "latex", booktabs = TRUE, longtable = TRUE,
             caption = "Per-regime summary: sample sizes, mean features, calibration slope with 95% CI, top-3 dominant leagues.",
             label = "tab:regime-summary") %>%
  kable_styling(latex_options = c("repeat_header"),
                full_width = FALSE)
writeLines(as.character(tex), file.path(tbl_dir, "table_3_4_regime_summary.tex"))

# -- Table 3.5: compact FLB-per-regime ------------------------------------
flb_compact <- slope_tbl %>%
  mutate(calibration_slope_95CI = sprintf("%.3f (%.3f, %.3f)", slope, slope_lo, slope_hi)) %>%
  select(regime_id = cluster, calibration_slope_95CI, slope) %>%
  arrange(slope) %>%
  select(-slope)

write_csv(flb_compact, file.path(tbl_dir, "table_3_5_flb_across_regimes.csv"))

tex <- kable(flb_compact, format = "latex", booktabs = TRUE,
             caption = "Calibration slope (realized on implied probability) per regime, with 95% CI. Sorted ascending.",
             label = "tab:flb-across-regimes") %>%
  kable_styling(latex_options = c("hold_position"))
writeLines(as.character(tex), file.path(tbl_dir, "table_3_5_flb_across_regimes.tex"))

# -- Console findings -----------------------------------------------------
min_idx <- which.min(slope_tbl$slope)
max_idx <- which.max(slope_tbl$slope)
ci_overlap <- !(slope_tbl$slope_lo[max_idx] > slope_tbl$slope_hi[min_idx])

cat("\n==== Section 3.5 findings ====\n")
cat(sprintf("1. Min slope: regime %s = %.3f (95%% CI %.3f, %.3f).\n",
            slope_tbl$cluster[min_idx], slope_tbl$slope[min_idx],
            slope_tbl$slope_lo[min_idx], slope_tbl$slope_hi[min_idx]))
cat(sprintf("2. Max slope: regime %s = %.3f (95%% CI %.3f, %.3f).\n",
            slope_tbl$cluster[max_idx], slope_tbl$slope[max_idx],
            slope_tbl$slope_lo[max_idx], slope_tbl$slope_hi[max_idx]))
cat(sprintf("3. Slope gap max - min = %.3f.\n",
            slope_tbl$slope[max_idx] - slope_tbl$slope[min_idx]))
cat(sprintf("4. Do min-regime and max-regime 95%% CIs overlap? %s  (disjoint CIs => significant heterogeneity).\n",
            ifelse(ci_overlap, "YES", "NO")))
cat(sprintf("5. Test-set assignments by regime: %s.\n",
            paste(sprintf("%s=%s", seq_len(K_star),
                          format(as.integer(table(test$cluster)), big.mark = ",")),
                  collapse = ", ")))
