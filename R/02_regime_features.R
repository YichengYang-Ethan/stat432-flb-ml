# 00_features.R
# Section 3.1 - Feature construction.
# Build 9 market-shape features from six-bookmaker de-vigged probabilities.
# Split into train (2013/14-2020/21) and test (2021/22-2024/25).

set.seed(441)
suppressPackageStartupMessages({
  library(tidyverse)
})

root      <- "~/Desktop/FLB_Final_Code"
data_dir  <- file.path(root, "data")

raw_path  <- "~/Desktop/FLB_Final_Code/data/analysis_sample.rds"
dat <- readRDS(raw_path)

# -- Compute 9 features ----------------------------------------------------
eps <- 1e-12
dat <- dat %>%
  mutate(
    p_fav      = pmax(q_H_mean, q_A_mean),
    p_long     = pmin(q_H_mean, q_A_mean),
    q_draw     = q_D_mean,
    entropy    = -(q_H_mean * log(pmax(q_H_mean, eps)) +
                   q_D_mean * log(pmax(q_D_mean, eps)) +
                   q_A_mean * log(pmax(q_A_mean, eps))),
    log_ratio  = log(p_fav / pmax(p_long, eps)),
    home_skew  = log(q_H_mean / pmax(q_A_mean, eps)),
    overround  = overround_mean,
    disagree_H = q_H_sd,
    disagree_A = q_A_sd
  )

feat_cols <- c("p_fav","p_long","q_draw","entropy","log_ratio",
               "home_skew","overround","disagree_H","disagree_A")

# -- Train / test split (by season_start) ----------------------------------
train_seasons <- 2013:2020
test_seasons  <- 2021:2024

train <- dat %>% filter(season_start %in% train_seasons)
test  <- dat %>% filter(season_start %in% test_seasons)

# -- Save ------------------------------------------------------------------
saveRDS(train, file.path(data_dir, "features_train.rds"))
saveRDS(test,  file.path(data_dir, "features_test.rds"))
write_csv(train %>% select(Date, Div, Season, season_start, Tier, FTR,
                           all_of(feat_cols),
                           q_H_mean, q_D_mean, q_A_mean,
                           overround_mean, q_H_sd, q_D_sd, q_A_sd),
          file.path(data_dir, "features_train.csv"))
write_csv(test %>% select(Date, Div, Season, season_start, Tier, FTR,
                          all_of(feat_cols),
                          q_H_mean, q_D_mean, q_A_mean,
                          overround_mean, q_H_sd, q_D_sd, q_A_sd),
          file.path(data_dir, "features_test.csv"))

# -- Summary ---------------------------------------------------------------
feat_summary <- train %>%
  select(all_of(feat_cols)) %>%
  pivot_longer(everything(), names_to = "feature", values_to = "value") %>%
  group_by(feature) %>%
  summarise(
    mean = mean(value),
    sd   = sd(value),
    min  = min(value),
    max  = max(value),
    .groups = "drop"
  ) %>%
  mutate(feature = factor(feature, levels = feat_cols)) %>%
  arrange(feature)

cat("==== Section 3.1 findings ====\n")
cat(sprintf("1. n_train = %s matches (seasons %d/%02d to %d/%02d).\n",
            format(nrow(train), big.mark = ","),
            min(train$season_start),     (min(train$season_start) + 1) %% 100,
            max(train$season_start),     (max(train$season_start) + 1) %% 100))
cat(sprintf("2. n_test  = %s matches (seasons %d/%02d to %d/%02d).\n",
            format(nrow(test), big.mark = ","),
            min(test$season_start),     (min(test$season_start) + 1) %% 100,
            max(test$season_start),     (max(test$season_start) + 1) %% 100))
cat(sprintf("3. Feature set: %d features - %s.\n",
            length(feat_cols), paste(feat_cols, collapse = ", ")))
cat("4. TRAIN feature summary (mean, sd, min, max):\n")
print(feat_summary %>% mutate(across(where(is.numeric), ~ signif(., 3))))
cat(sprintf("5. Train/test fraction: %.1f%% / %.1f%%.\n",
            nrow(train) / (nrow(train) + nrow(test)) * 100,
            nrow(test)  / (nrow(train) + nrow(test)) * 100))
