# 07_teamform.R
# Rolling team-form features (28 columns, 2 windows x 7 stats x 2 sides)
# with anti-leakage sanity checks; then build the modelling feature matrix,
# standardize on train, and split train/test.

# -----------------------------------------------------------------------------
# From 00_team_form.R
# -----------------------------------------------------------------------------

# Section 4.1a - Rolling team-form features (strictly-past windows).
#
# Strategy:
#   1. Build long per-(match, team, side) table with goals_for, goals_against,
#      result indicator, shots_on_target.
#   2. Sort by team + date, apply slider::slide_dbl with
#      .before = W, .after = -1, .complete = TRUE to get the mean over the W
#      matches strictly before the current one.
#   3. Join back to the match-level frame as home_* / away_* columns.

set.seed(441)
suppressPackageStartupMessages({
  library(tidyverse)
  library(slider)
})

root     <- "~/Desktop/FLB_Final_Code"
data_dir <- file.path(root, "data")

raw <- readRDS("~/Desktop/FLB_Final_Code/data/analysis_sample.rds")
stopifnot(nrow(raw) > 0)
cat("Loaded base sample:", nrow(raw), "matches.\n")

# Synthetic match id to allow joins without guessing
raw <- raw %>%
  arrange(Date, Div, HomeTeam, AwayTeam) %>%
  mutate(match_id = row_number())

# -- Long per-(match, team, side) table -----------------------------------
long <- bind_rows(
  raw %>% transmute(match_id, Date, Div, team = HomeTeam, side = "H",
                    gf = FTHG, ga = FTAG,
                    points = case_when(FTR == "H" ~ 3L,
                                       FTR == "D" ~ 1L,
                                       TRUE       ~ 0L),
                    win    = as.integer(FTR == "H"),
                    draw   = as.integer(FTR == "D"),
                    sot    = HST),
  raw %>% transmute(match_id, Date, Div, team = AwayTeam, side = "A",
                    gf = FTAG, ga = FTHG,
                    points = case_when(FTR == "A" ~ 3L,
                                       FTR == "D" ~ 1L,
                                       TRUE       ~ 0L),
                    win    = as.integer(FTR == "A"),
                    draw   = as.integer(FTR == "D"),
                    sot    = AST)
) %>%
  mutate(gd = gf - ga) %>%
  arrange(team, Date, match_id)

cat("Long rows:", nrow(long), " (expected 2 * n_matches =", 2 * nrow(raw), ")\n")

# -- Strict-past rolling helper -------------------------------------------
# For a vector x ordered within a team, return rolling mean over the W obs
# STRICTLY before the current position. Completeness is required (<W -> NA).
# na.rm = TRUE so missing entries (e.g. NA HST) do not poison the mean.
rollmean_past <- function(x, W, na.rm = FALSE) {
  slide_dbl(x,
            .f       = function(v) if (na.rm) mean(v, na.rm = TRUE) else mean(v),
            .before  = W,
            .after   = -1,
            .complete = TRUE)
}

# Strict-past count of non-NA values in the window (for shots-on-target diag)
rollcount_nonNA_past <- function(x, W) {
  slide_dbl(x,
            .f       = function(v) sum(!is.na(v)),
            .before  = W,
            .after   = -1,
            .complete = TRUE)
}

windows <- c(5L, 10L)

# Compute rolling features per team
roll <- long %>%
  group_by(team) %>%
  mutate(
    gf_5         = rollmean_past(gf, 5),
    ga_5         = rollmean_past(ga, 5),
    gd_5         = rollmean_past(gd, 5),
    win_rate_5   = rollmean_past(win, 5),
    draw_rate_5  = rollmean_past(draw, 5),
    ppg_5        = rollmean_past(points, 5),
    sot_5        = rollmean_past(sot, 5, na.rm = TRUE),
    sot_5_nobs   = rollcount_nonNA_past(sot, 5),

    gf_10        = rollmean_past(gf, 10),
    ga_10        = rollmean_past(ga, 10),
    gd_10        = rollmean_past(gd, 10),
    win_rate_10  = rollmean_past(win, 10),
    draw_rate_10 = rollmean_past(draw, 10),
    ppg_10       = rollmean_past(points, 10),
    sot_10       = rollmean_past(sot, 10, na.rm = TRUE),
    sot_10_nobs  = rollcount_nonNA_past(sot, 10)
  ) %>%
  ungroup()

# If the entire window has NA shots_on_target, mean() returns NaN; coerce to NA.
roll <- roll %>%
  mutate(sot_5  = ifelse(sot_5_nobs  == 0, NA_real_, sot_5),
         sot_10 = ifelse(sot_10_nobs == 0, NA_real_, sot_10)) %>%
  select(-sot_5_nobs, -sot_10_nobs)

# -- Verification tests ---------------------------------------------------
# (1) Man United 2014/15 E0, check home_gf_5 at matches 3, 5, 11 by hand
#     We'll inspect the long-table view directly.
cat("\n-- Sanity check 1: Man United 2014/15 (E0) rolling home_gf_5 --\n")
mu <- roll %>%
  filter(team == "Man United", Div == "E0") %>%
  arrange(Date)
if (nrow(mu) == 0) {
  cat("  NOTE: team label 'Man United' not found; peeking at available team names.\n")
  cat("  Sample E0 teams: ", paste(head(sort(unique(roll$team[roll$Div == "E0"])), 10), collapse = ", "), "\n")
} else {
  mu_s <- mu %>% filter(Date >= as.Date("2014-07-01"), Date <= as.Date("2015-06-30"))
  cat(sprintf("  MU 2014/15 has %d rows in long table.\n", nrow(mu_s)))
  check_idx <- c(3, 5, 11)
  for (i in check_idx) {
    if (i > nrow(mu_s)) next
    # Prior 5 rows in full (Div-agnostic) history
    full_hist <- mu %>% filter(Date < mu_s$Date[i]) %>% arrange(Date)
    manual_gf5 <- if (nrow(full_hist) >= 5) mean(tail(full_hist$gf, 5)) else NA_real_
    reported   <- mu_s$gf_5[i]
    cat(sprintf("  match #%d on %s: manual gf_5 = %s, reported = %s\n",
                i, as.character(mu_s$Date[i]),
                format(manual_gf5, digits = 4), format(reported, digits = 4)))
    stopifnot(identical(is.na(manual_gf5), is.na(reported)) ||
              isTRUE(all.equal(manual_gf5, reported, tolerance = 1e-8)))
  }
  cat("  PASS: slider output agrees with manual tail-mean on all checked indices.\n")
}

# (2) Temporal test - perturb a past match, re-compute, ensure only the
#     matches AFTER the perturbed date change.
cat("\n-- Sanity check 2: temporal independence --\n")
# Pick a team with long history
pick <- roll %>%
  group_by(team) %>%
  tally() %>%
  filter(n >= 30) %>%
  slice(1)
ptm <- pick$team
ptm_rows <- long %>% filter(team == ptm) %>% arrange(Date, match_id)
# Perturb gf at row 15
perturbed <- ptm_rows
perturbed$gf[15] <- 99
# Reroll
p_roll <- perturbed %>%
  mutate(gf_5_pert = rollmean_past(gf, 5))
base_vals <- ptm_rows %>%
  mutate(gf_5_base = rollmean_past(gf, 5))
a <- base_vals$gf_5_base
b <- p_roll$gf_5_pert
diff_rows <- which(xor(is.na(a), is.na(b)) |
                    (!is.na(a) & !is.na(b) & a != b))
if (length(diff_rows) > 0) {
  stopifnot(all(diff_rows > 15))
  cat(sprintf("  PASS: perturbation at row 15 only affects rows > 15 (first affected = %d, last = %d).\n",
              min(diff_rows), max(diff_rows)))
} else {
  cat("  WARN: perturbation had no downstream effect (row 15 may be near tail).\n")
}

# (3) Cold-start: first 5 rows per team should have NA in *_5 features
cat("\n-- Sanity check 3: cold start --\n")
cold5 <- roll %>%
  group_by(team) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  summarise(na_rate = mean(is.na(gf_5)))
cat(sprintf("  NA rate in gf_5 for first 5 rows per team: %.1f%% (expect ~100%%)\n",
            cold5$na_rate * 100))
stopifnot(cold5$na_rate > 0.99)
cat("  PASS.\n")

# -- Pivot long -> wide (home_* / away_*) ---------------------------------
feat_vars <- c("gf_5","ga_5","gd_5","win_rate_5","draw_rate_5","ppg_5","sot_5",
               "gf_10","ga_10","gd_10","win_rate_10","draw_rate_10","ppg_10","sot_10")

home_feats <- roll %>%
  filter(side == "H") %>%
  select(match_id, all_of(feat_vars)) %>%
  rename_with(~ paste0("home_", .), all_of(feat_vars))

away_feats <- roll %>%
  filter(side == "A") %>%
  select(match_id, all_of(feat_vars)) %>%
  rename_with(~ paste0("away_", .), all_of(feat_vars))

matches <- raw %>%
  left_join(home_feats, by = "match_id") %>%
  left_join(away_feats, by = "match_id")

# Sanity: exactly 28 new columns
new_cols <- c(paste0("home_", feat_vars), paste0("away_", feat_vars))
stopifnot(length(new_cols) == 28)
stopifnot(all(new_cols %in% names(matches)))
cat("\nMerged team-form features: ", length(new_cols), "new columns.\n")

# -- Drop matches with any team-form NA ----------------------------------
n_before <- nrow(matches)
matches_clean <- matches %>% filter(if_all(all_of(new_cols), ~ !is.na(.)))
n_after <- nrow(matches_clean)
cat(sprintf("Cold-start drop: %s -> %s  (%s dropped, %.1f%%).\n",
            format(n_before, big.mark = ","),
            format(n_after,  big.mark = ","),
            format(n_before - n_after, big.mark = ","),
            (n_before - n_after) / n_before * 100))

# Save the pre-drop version so the merge script can report counts.
saveRDS(matches, file.path(data_dir, "matches_with_teamform.rds"))

# -- Summary of new features (mean, sd) on NON-NA rows -------------------
feat_summary <- matches %>%
  select(all_of(new_cols)) %>%
  pivot_longer(everything(), names_to = "feature", values_to = "value") %>%
  group_by(feature) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            sd   = sd(value,   na.rm = TRUE),
            n    = sum(!is.na(value)),
            .groups = "drop")

cat("\n==== Section 4.1a findings ====\n")
cat(sprintf("1. Long table: %s rows (2 * n_matches).\n", format(nrow(long), big.mark = ",")))
cat(sprintf("2. Team-form features: 2 windows x 7 features x 2 sides = %d new columns.\n",
            length(new_cols)))
cat(sprintf("3. Cold-start drop: %s of %s matches (%.1f%%).\n",
            format(n_before - n_after, big.mark = ","),
            format(n_before, big.mark = ","),
            (n_before - n_after) / n_before * 100))
cat("4. Feature summary (mean, sd, n non-NA):\n")
print(feat_summary %>% mutate(across(c(mean, sd), ~ signif(., 3))) %>%
        arrange(feature), n = Inf)
cat("5. Saved: data/matches_with_teamform.rds (includes NA rows; drop happens in 01).\n")

# -----------------------------------------------------------------------------
# From 01_build_feature_matrix.R
# -----------------------------------------------------------------------------

# Section 4.1b - merge market + team-form + regime; standardize on train.

set.seed(441)
suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
})

root     <- "~/Desktop/FLB_Final_Code"
data_dir <- file.path(root, "data")
tbl_dir  <- file.path(root, "tables")

# -- Load inputs ----------------------------------------------------------
matches  <- readRDS(file.path(data_dir, "matches_with_teamform.rds"))

# Section 3 regime labels (train + test separately)
s3_train <- readRDS("~/Desktop/FLB_Final_Code/data/train_with_regime.rds")
s3_test  <- readRDS("~/Desktop/FLB_Final_Code/data/test_with_regime.rds")

# Combined regime map by join-key
regime_map <- bind_rows(
  s3_train %>% select(Date, HomeTeam, AwayTeam, cluster),
  s3_test  %>% select(Date, HomeTeam, AwayTeam, cluster)
) %>%
  rename(Regime = cluster)

# -- Join regimes ---------------------------------------------------------
matches <- matches %>%
  left_join(regime_map, by = c("Date", "HomeTeam", "AwayTeam"))

# Report regime-join coverage
n_no_regime <- sum(is.na(matches$Regime))
cat(sprintf("Matches without a Regime join: %s of %s (%.1f%%).\n",
            format(n_no_regime, big.mark = ","),
            format(nrow(matches), big.mark = ","),
            n_no_regime / nrow(matches) * 100))

# -- Feature columns ------------------------------------------------------
market_feats <- c("p_fav","p_long","q_draw","entropy","log_ratio",
                  "home_skew","overround_mean","q_H_sd","q_A_sd")
# Match section 3's feature names exactly. Section 3 used "overround" and
# "disagree_H" / "disagree_A"; in the base analysis file those are
# "overround_mean" / "q_H_sd" / "q_A_sd". Build thin aliases.
matches <- matches %>%
  mutate(p_fav      = pmax(q_H_mean, q_A_mean),
         p_long     = pmin(q_H_mean, q_A_mean),
         q_draw     = q_D_mean,
         entropy    = -(q_H_mean * log(q_H_mean) +
                        q_D_mean * log(q_D_mean) +
                        q_A_mean * log(q_A_mean)),
         log_ratio  = log(p_fav / p_long),
         home_skew  = log(q_H_mean / q_A_mean),
         overround  = overround_mean,
         disagree_H = q_H_sd,
         disagree_A = q_A_sd)
market_feats <- c("p_fav","p_long","q_draw","entropy","log_ratio",
                  "home_skew","overround","disagree_H","disagree_A")

teamform_feats <- c(
  paste0("home_", c("gf_5","ga_5","gd_5","win_rate_5","draw_rate_5","ppg_5","sot_5",
                    "gf_10","ga_10","gd_10","win_rate_10","draw_rate_10","ppg_10","sot_10")),
  paste0("away_", c("gf_5","ga_5","gd_5","win_rate_5","draw_rate_5","ppg_5","sot_5",
                    "gf_10","ga_10","gd_10","win_rate_10","draw_rate_10","ppg_10","sot_10"))
)
stopifnot(length(teamform_feats) == 28)
cont_feats <- c(market_feats, teamform_feats)
stopifnot(length(cont_feats) == 37)

# Odds columns to keep for Section 5 betting sim
odds_cols <- c("B365H","B365D","B365A","BWH","BWD","BWA",
               "IWH","IWD","IWA","WHH","WHD","WHA",
               "VCH","VCD","VCA","PSCH","PSCD","PSCA")

# -- Drop cold-start NA rows ---------------------------------------------
n_before <- nrow(matches)
clean <- matches %>%
  filter(if_all(all_of(teamform_feats), ~ !is.na(.))) %>%
  filter(!is.na(Regime))
n_after <- nrow(clean)
drop_rate <- (n_before - n_after) / n_before * 100
cat(sprintf("Dropped %s matches (cold-start + missing regime): %.1f%%.  Final n = %s.\n",
            format(n_before - n_after, big.mark = ","),
            drop_rate,
            format(n_after, big.mark = ",")))

# -- Train / test split ---------------------------------------------------
train_seasons <- 2013:2020
test_seasons  <- 2021:2024

train <- clean %>% filter(season_start %in% train_seasons)
test  <- clean %>% filter(season_start %in% test_seasons)

cat(sprintf("n_train = %s, n_test = %s.\n",
            format(nrow(train), big.mark = ","),
            format(nrow(test),  big.mark = ",")))

# -- Standardize continuous features on TRAIN ONLY ------------------------
X_train <- as.matrix(train[, cont_feats])
ctr <- colMeans(X_train)
scl <- apply(X_train, 2, sd)
# Guard against zero SD
scl[scl == 0] <- 1

standardize <- function(df) {
  X <- as.matrix(df[, cont_feats])
  Xz <- sweep(sweep(X, 2, ctr, "-"), 2, scl, "/")
  as_tibble(Xz)
}
Z_train <- standardize(train)
Z_test  <- standardize(test)

saveRDS(list(center = ctr, scale = scl, cont_feats = cont_feats),
        file.path(data_dir, "feature_scaling.rds"))

# -- Final tibbles (keys + FTR factor + Regime + standardized feats + odds)
make_final <- function(src, Z) {
  bind_cols(
    src %>% transmute(
      match_id,
      Date, Div, Season, season_start, HomeTeam, AwayTeam,
      FTR = factor(FTR, levels = c("H","D","A")),
      Regime = factor(Regime, levels = c("1","2","3","4"))
    ),
    Z,
    src %>% select(all_of(odds_cols)),
    # Keep raw de-vigged means for baselines
    src %>% transmute(q_H_mean, q_D_mean, q_A_mean,
                      overround_mean)
  )
}
train_out <- make_final(train, Z_train)
test_out  <- make_final(test,  Z_test)

saveRDS(train_out, file.path(data_dir, "train_features.rds"))
saveRDS(test_out,  file.path(data_dir, "test_features.rds"))

# -- Feature-list table ---------------------------------------------------
describe_feat <- function(f) {
  if (f == "p_fav")      "Max of home / away de-vigged prob"
  else if (f == "p_long") "Min of home / away de-vigged prob"
  else if (f == "q_draw") "Draw de-vigged prob"
  else if (f == "entropy") "Shannon entropy of (qH, qD, qA)"
  else if (f == "log_ratio") "log(p_fav / p_long)"
  else if (f == "home_skew") "log(qH / qA)"
  else if (f == "overround") "Mean overround across six books"
  else if (f == "disagree_H") "SD of qH across six books"
  else if (f == "disagree_A") "SD of qA across six books"
  else if (grepl("^home_gf", f))        sprintf("Rolling home-team goals for  (last %s)",   sub(".*_", "", f))
  else if (grepl("^home_ga", f))        sprintf("Rolling home-team goals against (last %s)", sub(".*_", "", f))
  else if (grepl("^home_gd", f))        sprintf("Rolling home-team goal diff    (last %s)",   sub(".*_", "", f))
  else if (grepl("^home_win_rate", f))  sprintf("Rolling home-team win rate     (last %s)",   sub(".*_", "", f))
  else if (grepl("^home_draw_rate", f)) sprintf("Rolling home-team draw rate    (last %s)",   sub(".*_", "", f))
  else if (grepl("^home_ppg", f))       sprintf("Rolling home-team points/game  (last %s)",   sub(".*_", "", f))
  else if (grepl("^home_sot", f))       sprintf("Rolling home-team shots-on-target avg (last %s)", sub(".*_", "", f))
  else if (grepl("^away_gf", f))        sprintf("Rolling away-team goals for   (last %s)",   sub(".*_", "", f))
  else if (grepl("^away_ga", f))        sprintf("Rolling away-team goals against(last %s)",   sub(".*_", "", f))
  else if (grepl("^away_gd", f))        sprintf("Rolling away-team goal diff    (last %s)",   sub(".*_", "", f))
  else if (grepl("^away_win_rate", f))  sprintf("Rolling away-team win rate     (last %s)",   sub(".*_", "", f))
  else if (grepl("^away_draw_rate", f)) sprintf("Rolling away-team draw rate    (last %s)",   sub(".*_", "", f))
  else if (grepl("^away_ppg", f))       sprintf("Rolling away-team points/game  (last %s)",   sub(".*_", "", f))
  else if (grepl("^away_sot", f))       sprintf("Rolling away-team shots-on-target avg (last %s)", sub(".*_", "", f))
  else f
}

feature_list <- tibble(
  feature     = c(cont_feats, "Regime"),
  group       = c(rep("Market",   length(market_feats)),
                  rep("TeamForm", length(teamform_feats)),
                  "Regime"),
  description = c(sapply(cont_feats, describe_feat),
                  "Unsupervised K-Means cluster from Section 3 (1-4)")
)

write_csv(feature_list, file.path(tbl_dir, "table_4_1_feature_list.csv"))
tex <- kable(feature_list, format = "latex", booktabs = TRUE, longtable = TRUE,
             caption = "Modelling feature list: 9 market-shape, 28 team-form, 1 regime label.",
             label = "tab:feature-list") %>%
  kable_styling(latex_options = c("repeat_header"), full_width = FALSE)
writeLines(as.character(tex), file.path(tbl_dir, "table_4_1_feature_list.tex"))

# -- Findings -------------------------------------------------------------
cat("\n==== Section 4.1b findings ====\n")
cat(sprintf("1. n_train = %s, n_test = %s.\n",
            format(nrow(train_out), big.mark = ","),
            format(nrow(test_out),  big.mark = ",")))
cat(sprintf("2. Dropped %s of %s matches (%.1f%%) due to cold start / missing regime.\n",
            format(n_before - n_after, big.mark = ","),
            format(n_before, big.mark = ","),
            drop_rate))
cat(sprintf("3. Standardization center/scale saved; shapes: %d features.\n",
            length(cont_feats)))
cat(sprintf("4. Regime test-set distribution: %s.\n",
            paste(sprintf("%s=%s", levels(test_out$Regime),
                          format(as.integer(table(test_out$Regime)), big.mark = ",")),
                  collapse = ", ")))
cat(sprintf("5. Season range: train = %s..%s, test = %s..%s.\n",
            min(train_out$Season), max(train_out$Season),
            min(test_out$Season),  max(test_out$Season)))
