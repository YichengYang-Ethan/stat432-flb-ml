# 01_eda.R
# Consolidated Section 2 EDA (sample, overround, pooled FLB,
# heterogeneity, cross-book disagreement).
# PDF output stripped; PNG only.

# -----------------------------------------------------------------------------
# From 01_sample_and_outcomes.R
# -----------------------------------------------------------------------------

# Section 2.1 - describe the analysis sample.

suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
})

root     <- "~/Desktop/FLB_Final_Code"
data_dir <- file.path(root, "data")
fig_dir  <- file.path(root, "figures")
tbl_dir  <- file.path(root, "tables")

dat <- readRDS(file.path(data_dir, "analysis_sample.rds"))

# League name / country map
country_map <- tribble(
  ~Div, ~Country,
  "E0", "England (Premier League)",
  "E1", "England (Championship)",
  "E2", "England (League One)",
  "E3", "England (League Two)",
  "EC", "England (Conference)",
  "D1", "Germany (Bundesliga)",
  "D2", "Germany (2. Bundesliga)",
  "I1", "Italy (Serie A)",
  "I2", "Italy (Serie B)",
  "SP1","Spain (La Liga)",
  "SP2","Spain (Segunda Division)",
  "F1", "France (Ligue 1)",
  "F2", "France (Ligue 2)",
  "N1", "Netherlands (Eredivisie)",
  "B1", "Belgium (Pro League)",
  "P1", "Portugal (Primeira Liga)",
  "T1", "Turkey (Super Lig)",
  "G1", "Greece (Super League)",
  "SC0","Scotland (Premiership)",
  "SC1","Scotland (Championship)",
  "SC2","Scotland (League One)",
  "SC3","Scotland (League Two)"
)

# Train / test split by season_start
train_seasons <- 2013:2020  # 2013/14 .. 2020/21
test_seasons  <- 2021:2024  # 2021/22 .. 2024/25

tbl <- dat %>%
  mutate(
    is_train = season_start %in% train_seasons,
    is_test  = season_start %in% test_seasons
  ) %>%
  group_by(Div) %>%
  summarise(
    n_matches = n(),
    H_pct     = mean(FTR == "H") * 100,
    D_pct     = mean(FTR == "D") * 100,
    A_pct     = mean(FTR == "A") * 100,
    n_train   = sum(is_train),
    n_test    = sum(is_test),
    .groups   = "drop"
  ) %>%
  left_join(country_map, by = "Div") %>%
  mutate(Country = coalesce(Country, Div)) %>%
  select(Div, Country, n_matches, H_pct, D_pct, A_pct, n_train, n_test) %>%
  arrange(desc(n_matches))

# Pretty-format for presentation
tbl_fmt <- tbl %>%
  mutate(
    `H%` = sprintf("%.1f", H_pct),
    `D%` = sprintf("%.1f", D_pct),
    `A%` = sprintf("%.1f", A_pct)
  ) %>%
  select(Div, Country, n_matches, `H%`, `D%`, `A%`, n_train, n_test)

# -- Save tables ------------------------------------------------------------
write_csv(tbl_fmt, file.path(tbl_dir, "table_2_1_sample_by_league.csv"))

tex <- kable(tbl_fmt, format = "latex", booktabs = TRUE,
             caption = "Per-league sample breakdown.",
             label   = "tab:sample-by-league",
             col.names = c("Div","Country","n matches","H\\%","D\\%","A\\%","n train","n test"),
             escape = FALSE) %>%
  kable_styling(latex_options = c("hold_position"))
writeLines(as.character(tex), file.path(tbl_dir, "table_2_1_sample_by_league.tex"))

# -- Console findings -------------------------------------------------------
overall <- dat %>%
  summarise(H = mean(FTR == "H"), D = mean(FTR == "D"), A = mean(FTR == "A"))
n_train_total <- sum(dat$season_start %in% train_seasons)
n_test_total  <- sum(dat$season_start %in% test_seasons)

cat("==== Section 2.1 findings ====\n")
cat(sprintf("1. Analysis sample: %d matches across %d leagues, %d seasons (%s to %s).\n",
            nrow(dat), n_distinct(dat$Div),
            n_distinct(dat$Season), min(dat$Season), max(dat$Season)))
cat(sprintf("2. Overall FTR base rates: H = %.2f%%, D = %.2f%%, A = %.2f%%.\n",
            overall$H * 100, overall$D * 100, overall$A * 100))
cat(sprintf("3. Home wins outnumber away wins by %.1f percentage points (home-field edge).\n",
            (overall$H - overall$A) * 100))
cat(sprintf("4. Train split (2013/14-2020/21): %d matches (%.1f%%).  Test split (2021/22-2024/25): %d matches (%.1f%%).\n",
            n_train_total, n_train_total / nrow(dat) * 100,
            n_test_total,  n_test_total  / nrow(dat) * 100))
cat(sprintf("5. Largest league by volume: %s (%s) with %d matches; smallest: %s with %d.\n",
            tbl$Div[1], tbl$Country[1], tbl$n_matches[1],
            tbl$Div[nrow(tbl)], tbl$n_matches[nrow(tbl)]))
cat("\nTable 2.1 (sorted by n_matches):\n")
print(tbl_fmt)

# -----------------------------------------------------------------------------
# From 02_overround.R
# -----------------------------------------------------------------------------

# Section 2.2 - bookmaker margin (overround) across leagues.

suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
})

root     <- "~/Desktop/FLB_Final_Code"
data_dir <- file.path(root, "data")
fig_dir  <- file.path(root, "figures")
tbl_dir  <- file.path(root, "tables")

dat <- readRDS(file.path(data_dir, "analysis_sample.rds"))

theme_eda <- theme_bw() + theme(panel.grid.minor = element_blank())

save_fig <- function(p, name, w = 8, h = 5) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 300)
}

# -- Figure 2.1: histogram of overround_mean -------------------------------
p1 <- ggplot(dat, aes(x = overround_mean)) +
  geom_histogram(binwidth = 0.002, fill = "steelblue", colour = "white", linewidth = 0.2) +
  geom_vline(xintercept = median(dat$overround_mean), linetype = "dashed", colour = "firebrick") +
  coord_cartesian(xlim = c(1.00, 1.15)) +
  labs(x = "Mean overround across six bookmakers",
       y = "Number of matches",
       title = "Figure 2.1 - Distribution of bookmaker overround",
       subtitle = sprintf("Dashed line at median = %.4f", median(dat$overround_mean))) +
  theme_eda

save_fig(p1, "figure_2_1_overround_hist")

# -- Figure 2.2: boxplot by Div ordered by median --------------------------
ord <- dat %>%
  group_by(Div) %>%
  summarise(med = median(overround_mean), Tier = first(Tier), .groups = "drop") %>%
  arrange(med)

dat_plot <- dat %>% mutate(Div = factor(Div, levels = ord$Div))

p2 <- ggplot(dat_plot, aes(x = Div, y = overround_mean, fill = Tier)) +
  geom_boxplot(outlier.size = 0.4, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("Top" = "#2c7fb8", "Second" = "#fdae61")) +
  coord_cartesian(ylim = c(1.00, 1.15)) +
  labs(x = "League (Div code, ordered by median overround)",
       y = "Mean overround across six bookmakers",
       title = "Figure 2.2 - Overround by league",
       fill = "Tier") +
  theme_eda

save_fig(p2, "figure_2_2_overround_by_league")

# -- Summary table ---------------------------------------------------------
overall_summary <- dat %>%
  summarise(
    n      = n(),
    median = median(overround_mean),
    q25    = quantile(overround_mean, 0.25),
    q75    = quantile(overround_mean, 0.75),
    IQR    = q75 - q25
  )

tier_summary <- dat %>%
  group_by(Tier) %>%
  summarise(
    n      = n(),
    median = median(overround_mean),
    q25    = quantile(overround_mean, 0.25),
    q75    = quantile(overround_mean, 0.75),
    IQR    = q75 - q25,
    .groups = "drop"
  )

league_summary <- dat %>%
  group_by(Div, Tier) %>%
  summarise(
    n      = n(),
    median = median(overround_mean),
    q25    = quantile(overround_mean, 0.25),
    q75    = quantile(overround_mean, 0.75),
    IQR    = q75 - q25,
    .groups = "drop"
  ) %>%
  arrange(median)

write_csv(league_summary, file.path(tbl_dir, "table_2_2_overround_by_league.csv"))

tex <- kable(league_summary %>%
               mutate(across(c(median, q25, q75, IQR), ~ sprintf("%.4f", .))),
             format = "latex", booktabs = TRUE,
             caption = "Overround (S) summary by league, ordered by median.",
             label   = "tab:overround-by-league") %>%
  kable_styling(latex_options = c("hold_position"))
writeLines(as.character(tex), file.path(tbl_dir, "table_2_2_overround_by_league.tex"))

# -- Console findings -------------------------------------------------------
cat("==== Section 2.2 findings ====\n")
cat(sprintf("1. Overall overround: median = %.4f, IQR = [%.4f, %.4f] (width %.4f).\n",
            overall_summary$median, overall_summary$q25, overall_summary$q75, overall_summary$IQR))
for (i in seq_len(nrow(tier_summary))) {
  cat(sprintf("2%s. Tier = %-6s : median = %.4f, IQR = [%.4f, %.4f].\n",
              letters[i],
              tier_summary$Tier[i], tier_summary$median[i],
              tier_summary$q25[i], tier_summary$q75[i]))
}
cat(sprintf("3. Tightest book (lowest median overround): %s = %.4f.\n",
            league_summary$Div[1], league_summary$median[1]))
cat(sprintf("4. Widest book (highest median overround): %s = %.4f.\n",
            league_summary$Div[nrow(league_summary)], league_summary$median[nrow(league_summary)]))
cat(sprintf("5. Top-tier median minus Second-tier median overround: %.4f (Top is %s).\n",
            tier_summary$median[tier_summary$Tier == "Top"] -
              tier_summary$median[tier_summary$Tier == "Second"],
            ifelse(tier_summary$median[tier_summary$Tier == "Top"] <
                     tier_summary$median[tier_summary$Tier == "Second"],
                   "tighter", "wider")))

# -----------------------------------------------------------------------------
# From 03_flb_aggregate.R
# -----------------------------------------------------------------------------

# Section 2.3 - pooled reliability diagram for evidence of FLB.

suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
})

root     <- "~/Desktop/FLB_Final_Code"
data_dir <- file.path(root, "data")
fig_dir  <- file.path(root, "figures")
tbl_dir  <- file.path(root, "tables")

dat <- readRDS(file.path(data_dir, "analysis_sample.rds"))

theme_eda <- theme_bw() + theme(panel.grid.minor = element_blank())

save_fig <- function(p, name, w = 8, h = 5) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 300)
}

# -- Step 1: long format (implied_prob, outcome) -------------------------
long <- bind_rows(
  tibble(p = dat$q_H_mean, y = as.integer(dat$FTR == "H"), outcome = "H"),
  tibble(p = dat$q_D_mean, y = as.integer(dat$FTR == "D"), outcome = "D"),
  tibble(p = dat$q_A_mean, y = as.integer(dat$FTR == "A"), outcome = "A")
)
cat("Long-format rows:", nrow(long), "  (should be 3 x n =", 3 * nrow(dat), ")\n")

# -- Step 2: 20 equal-width bins over [0,1] -------------------------------
n_bins <- 20
breaks <- seq(0, 1, length.out = n_bins + 1)
long <- long %>%
  mutate(bin = cut(p, breaks = breaks, include.lowest = TRUE, right = FALSE))

wilson_ci <- function(k, n, z = 1.96) {
  if (n == 0) return(c(NA_real_, NA_real_))
  phat   <- k / n
  denom  <- 1 + z^2 / n
  centre <- (phat + z^2 / (2 * n)) / denom
  halfw  <- (z * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2))) / denom
  c(centre - halfw, centre + halfw)
}

# -- Step 3: per-bin summary ----------------------------------------------
bin_tbl <- long %>%
  group_by(bin) %>%
  summarise(
    mean_p   = mean(p),
    realized = mean(y),
    k        = sum(y),
    n        = n(),
    .groups  = "drop"
  ) %>%
  filter(n > 0) %>%
  rowwise() %>%
  mutate(
    ci_lo = wilson_ci(k, n)[1],
    ci_hi = wilson_ci(k, n)[2]
  ) %>%
  ungroup()

write_csv(bin_tbl, file.path(tbl_dir, "table_2_3_reliability_bins.csv"))

tex <- kable(bin_tbl %>%
               mutate(across(c(mean_p, realized, ci_lo, ci_hi), ~ sprintf("%.4f", .))),
             format = "latex", booktabs = TRUE,
             caption = "Reliability-diagram bins (20 equal-width).",
             label   = "tab:flb-bins") %>%
  kable_styling(latex_options = c("hold_position"))
writeLines(as.character(tex), file.path(tbl_dir, "table_2_3_reliability_bins.tex"))

# -- Step 4: Figure 2.3 ---------------------------------------------------
p3 <- ggplot(bin_tbl, aes(x = mean_p, y = realized)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.015,
                colour = "grey30", alpha = 0.7) +
  geom_point(aes(size = n), colour = "firebrick", alpha = 0.85) +
  scale_size_continuous(name = "Bin count", range = c(1.5, 6)) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x     = "Implied probability (de-vigged)",
       y     = "Realized frequency",
       title = "Figure 2.3 - Reliability diagram (pooled H, D, A)",
       subtitle = "Dashed line: perfect calibration; error bars: Wilson 95% CI") +
  theme_eda

save_fig(p3, "figure_2_3_reliability_pooled")

# -- Linear regression slope (OLS, weighted by bin n) ---------------------
fit <- lm(realized ~ mean_p, data = bin_tbl, weights = n)
slope     <- unname(coef(fit)[2])
intercept <- unname(coef(fit)[1])

# -- Signed deviations in lowest / highest implied-prob quintiles ---------
quint <- long %>%
  mutate(q5 = ntile(p, 5)) %>%
  group_by(q5) %>%
  summarise(
    mean_p   = mean(p),
    realized = mean(y),
    dev      = realized - mean_p,
    n        = n(),
    .groups  = "drop"
  )

# -- Console --------------------------------------------------------------
cat("==== Section 2.3 findings ====\n")
cat(sprintf("1. Pooled reliability fit: realized = %.3f + %.3f * implied  (slope > 1 => FLB).\n",
            intercept, slope))
cat(sprintf("2. Slope = %.3f; deviation from 1 is %+.3f.\n", slope, slope - 1))
cat(sprintf("3. Lowest quintile (longshots): mean implied = %.3f, realized = %.3f, signed dev = %+.4f.\n",
            quint$mean_p[1], quint$realized[1], quint$dev[1]))
cat(sprintf("4. Highest quintile (favourites): mean implied = %.3f, realized = %.3f, signed dev = %+.4f.\n",
            quint$mean_p[5], quint$realized[5], quint$dev[5]))
cat(sprintf("5. FLB sign check: longshots %s, favourites %s (expected under FLB: longshots negative, favourites positive).\n",
            ifelse(quint$dev[1] < 0, "realized BELOW implied", "realized ABOVE implied"),
            ifelse(quint$dev[5] > 0, "realized ABOVE implied", "realized BELOW implied")))
cat("\nFull quintile table:\n"); print(quint)
cat("\nBin table:\n"); print(bin_tbl, n = Inf)

# -----------------------------------------------------------------------------
# From 04_flb_heterogeneity.R
# -----------------------------------------------------------------------------

# Section 2.4 - heterogeneity of FLB across sub-populations.

suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
})

root     <- "~/Desktop/FLB_Final_Code"
data_dir <- file.path(root, "data")
fig_dir  <- file.path(root, "figures")
tbl_dir  <- file.path(root, "tables")

dat <- readRDS(file.path(data_dir, "analysis_sample.rds"))

theme_eda <- theme_bw() + theme(panel.grid.minor = element_blank())

save_fig <- function(p, name, w = 9, h = 6) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 300)
}

wilson_ci <- function(k, n, z = 1.96) {
  if (n == 0) return(c(NA_real_, NA_real_))
  phat   <- k / n
  denom  <- 1 + z^2 / n
  centre <- (phat + z^2 / (2 * n)) / denom
  halfw  <- (z * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2))) / denom
  c(centre - halfw, centre + halfw)
}

# -- Add match-level grouping variables ------------------------------------
dat <- dat %>%
  mutate(
    overround_q = ntile(overround_mean, 4),
    disagree    = q_H_sd + q_A_sd,
    disagree_q  = ntile(disagree, 4)
  )

# -- Build long-format (H, D, A) table with group labels attached ----------
build_long <- function(d) {
  bind_rows(
    d %>% transmute(p = q_H_mean, y = as.integer(FTR == "H"), Tier, overround_q, disagree_q),
    d %>% transmute(p = q_D_mean, y = as.integer(FTR == "D"), Tier, overround_q, disagree_q),
    d %>% transmute(p = q_A_mean, y = as.integer(FTR == "A"), Tier, overround_q, disagree_q)
  )
}
long <- build_long(dat)

# -- Per-group bin summary ---------------------------------------------------
n_bins <- 20
breaks <- seq(0, 1, length.out = n_bins + 1)

bin_summary <- function(df, group_col) {
  df %>%
    mutate(bin = cut(p, breaks = breaks, include.lowest = TRUE, right = FALSE)) %>%
    group_by(.data[[group_col]], bin) %>%
    summarise(
      mean_p   = mean(p),
      realized = mean(y),
      k        = sum(y),
      n        = n(),
      .groups  = "drop"
    ) %>%
    filter(n > 0) %>%
    rowwise() %>%
    mutate(
      ci_lo = wilson_ci(k, n)[1],
      ci_hi = wilson_ci(k, n)[2]
    ) %>%
    ungroup()
}

# OLS slope per panel, weighted by n
panel_slope <- function(df, group_col) {
  df %>%
    group_by(.data[[group_col]]) %>%
    group_modify(~ {
      fit <- lm(realized ~ mean_p, data = .x, weights = .x$n)
      tibble(slope     = unname(coef(fit)[2]),
             intercept = unname(coef(fit)[1]),
             n_obs     = sum(.x$n))
    }) %>%
    ungroup()
}

panel_plot <- function(bin_tbl, slope_tbl, group_col, title_str) {
  # Build facet label of form "value   slope = X.XX"
  slope_tbl <- slope_tbl %>%
    mutate(label = sprintf("%s: slope = %.3f",
                           as.character(.data[[group_col]]), slope))
  lookup <- setNames(slope_tbl$label, as.character(slope_tbl[[group_col]]))

  ggplot(bin_tbl, aes(x = mean_p, y = realized)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.02,
                  colour = "grey30", alpha = 0.6) +
    geom_point(aes(size = n), colour = "firebrick", alpha = 0.85) +
    scale_size_continuous(name = "Bin count", range = c(1, 4)) +
    coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
    facet_wrap(vars(.data[[group_col]]),
               labeller = as_labeller(lookup)) +
    labs(x = "Implied probability (de-vigged)",
         y = "Realized frequency",
         title = title_str) +
    theme_eda
}

# -- 2.4a: Tier ------------------------------------------------------------
bin_tier   <- bin_summary(long, "Tier")
slope_tier <- panel_slope(bin_tier, "Tier")
p4a <- panel_plot(bin_tier, slope_tier, "Tier",
                  "Figure 2.4a - Reliability by Tier (Top vs Second)")
save_fig(p4a, "figure_2_4a_flb_by_tier", w = 9, h = 5)

# -- 2.4b: Overround quartile ---------------------------------------------
long2 <- long %>% mutate(overround_q = factor(overround_q,
  levels = 1:4,
  labels = c("Q1 (low margin)","Q2","Q3","Q4 (high margin)")))
bin_over   <- bin_summary(long2, "overround_q")
slope_over <- panel_slope(bin_over, "overround_q")
p4b <- panel_plot(bin_over, slope_over, "overround_q",
                  "Figure 2.4b - Reliability by overround quartile")
save_fig(p4b, "figure_2_4b_flb_by_overround", w = 10, h = 7)

# -- 2.4c: Book-disagreement quartile -------------------------------------
long3 <- long %>% mutate(disagree_q = factor(disagree_q,
  levels = 1:4,
  labels = c("Q1 (low disagree)","Q2","Q3","Q4 (high disagree)")))
bin_dis   <- bin_summary(long3, "disagree_q")
slope_dis <- panel_slope(bin_dis, "disagree_q")
p4c <- panel_plot(bin_dis, slope_dis, "disagree_q",
                  "Figure 2.4c - Reliability by book-disagreement quartile")
save_fig(p4c, "figure_2_4c_flb_by_disagreement", w = 10, h = 7)

# -- Combined slope table -------------------------------------------------
slope_all <- bind_rows(
  slope_tier %>% transmute(facet = "Tier",          group = as.character(Tier),        slope, intercept, n_obs),
  slope_over %>% transmute(facet = "Overround Q",   group = as.character(overround_q), slope, intercept, n_obs),
  slope_dis  %>% transmute(facet = "Disagreement Q",group = as.character(disagree_q),  slope, intercept, n_obs)
)
write_csv(slope_all, file.path(tbl_dir, "table_2_4_flb_slopes.csv"))

tex <- kable(slope_all %>%
               mutate(slope = sprintf("%.3f", slope),
                      intercept = sprintf("%.3f", intercept)),
             format = "latex", booktabs = TRUE,
             caption = "Fitted reliability slopes by sub-population. Slope > 1 implies FLB.",
             label   = "tab:flb-slopes") %>%
  kable_styling(latex_options = c("hold_position"))
writeLines(as.character(tex), file.path(tbl_dir, "table_2_4_flb_slopes.tex"))

# -- Console --------------------------------------------------------------
cat("==== Section 2.4 findings ====\n")
cat("1. Tier slopes:\n"); print(slope_tier)
cat("2. Overround-quartile slopes:\n"); print(slope_over)
cat("3. Disagreement-quartile slopes:\n"); print(slope_dis)

t_top <- slope_tier$slope[slope_tier$Tier == "Top"]
t_sec <- slope_tier$slope[slope_tier$Tier == "Second"]
cat(sprintf("4. Tier FLB gap: Second slope = %.3f vs Top slope = %.3f  (diff = %+.3f).\n",
            t_sec, t_top, t_sec - t_top))

o_lo <- slope_over$slope[slope_over$overround_q == "Q1 (low margin)"]
o_hi <- slope_over$slope[slope_over$overround_q == "Q4 (high margin)"]
cat(sprintf("5. Overround FLB gap: Q4 (high-margin) slope = %.3f vs Q1 (low-margin) slope = %.3f (diff = %+.3f).\n",
            o_hi, o_lo, o_hi - o_lo))

d_lo <- slope_dis$slope[slope_dis$disagree_q == "Q1 (low disagree)"]
d_hi <- slope_dis$slope[slope_dis$disagree_q == "Q4 (high disagree)"]
cat(sprintf("6. Disagreement FLB gap: Q4 (high-disagree) slope = %.3f vs Q1 (low-disagree) slope = %.3f (diff = %+.3f).\n",
            d_hi, d_lo, d_hi - d_lo))

# -----------------------------------------------------------------------------
# From 05_book_disagreement.R
# -----------------------------------------------------------------------------

# Section 2.5 - cross-bookmaker disagreement.

suppressPackageStartupMessages({
  library(tidyverse)
  library(hexbin)
  library(kableExtra)
})

root     <- "~/Desktop/FLB_Final_Code"
data_dir <- file.path(root, "data")
fig_dir  <- file.path(root, "figures")
tbl_dir  <- file.path(root, "tables")

dat <- readRDS(file.path(data_dir, "analysis_sample.rds"))

theme_eda <- theme_bw() + theme(panel.grid.minor = element_blank())

save_fig <- function(p, name, w = 8, h = 5) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 300)
}

# -- Figure 2.5: hex-bin of q_H_sd vs q_H_mean + LOESS --------------------
p5 <- ggplot(dat, aes(x = q_H_mean, y = q_H_sd)) +
  geom_hex(bins = 50) +
  geom_smooth(method = "loess", se = FALSE, colour = "firebrick", linewidth = 0.8) +
  scale_fill_viridis_c(option = "C", trans = "log10", name = "Count (log10)") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, max(dat$q_H_sd) * 1.05)) +
  labs(x = "Mean implied home-win probability (de-vigged)",
       y = "SD across six bookmakers",
       title = "Figure 2.5 - Cross-book disagreement on home-win probability",
       subtitle = "Hex density with LOESS fit (red)") +
  theme_eda

save_fig(p5, "figure_2_5_disagreement_hex")

# -- Summary statistics ---------------------------------------------------
overall_mean   <- mean(dat$q_H_sd)
overall_median <- median(dat$q_H_sd)

anchors <- c(0.1, 0.3, 0.5, 0.7, 0.9)
window  <- 0.02  # +/- width around each anchor

anchor_tbl <- map_dfr(anchors, function(a) {
  sub <- dat %>% filter(q_H_mean >= a - window, q_H_mean <= a + window)
  tibble(anchor = a,
         n      = nrow(sub),
         mean_sd   = mean(sub$q_H_sd),
         median_sd = median(sub$q_H_sd))
})

write_csv(anchor_tbl, file.path(tbl_dir, "table_2_5_disagreement_by_anchor.csv"))

tex <- kable(anchor_tbl %>%
               mutate(across(c(mean_sd, median_sd), ~ sprintf("%.5f", .))),
             format = "latex", booktabs = TRUE,
             caption = sprintf("Cross-book SD of implied home-win probability at anchor values (+/- %.2f window).",
                               window),
             label   = "tab:disagreement-anchors") %>%
  kable_styling(latex_options = c("hold_position"))
writeLines(as.character(tex), file.path(tbl_dir, "table_2_5_disagreement_by_anchor.tex"))

# -- Console --------------------------------------------------------------
cat("==== Section 2.5 findings ====\n")
cat(sprintf("1. Overall q_H_sd: mean = %.5f, median = %.5f (n = %d).\n",
            overall_mean, overall_median, nrow(dat)))
cat("2. Disagreement by anchor (window +/- 0.02):\n")
print(anchor_tbl %>%
        mutate(across(c(mean_sd, median_sd), ~ sprintf("%.5f", .))))

peak <- anchor_tbl$anchor[which.max(anchor_tbl$mean_sd)]
trough <- anchor_tbl$anchor[which.min(anchor_tbl$mean_sd)]
monotone <- all(diff(anchor_tbl$mean_sd) > 0)
cat(sprintf("3. Disagreement is highest near q_H_mean = %.2f (mean SD = %.5f).\n",
            peak, max(anchor_tbl$mean_sd)))
cat(sprintf("4. Disagreement is smallest near q_H_mean = %.2f (mean SD = %.5f).\n",
            trough, min(anchor_tbl$mean_sd)))
cat(sprintf("5. Ratio peak/trough mean SD: %.2fx%s.\n",
            max(anchor_tbl$mean_sd) / min(anchor_tbl$mean_sd),
            ifelse(monotone, "; SD grows monotonically with p_H across the five anchors", "")))
