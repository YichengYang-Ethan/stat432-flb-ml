# 00_prepare_data.R
# Load raw data, filter to analysis window, build implied-probability features.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

root      <- "~/Desktop/FLB_Final_Code"
raw_path  <- "~/Desktop/full_dataset.csv"
data_dir  <- file.path(root, "data")

# -- Load -------------------------------------------------------------------
raw <- read_csv(raw_path, show_col_types = FALSE, guess_max = 100000)
cat("Raw rows:", nrow(raw), " Raw cols:", ncol(raw), "\n")

# -- Parse date and filter to analysis window -------------------------------
dat <- raw %>%
  mutate(Date = dmy(Date)) %>%
  filter(!is.na(Date),
         Date >= ymd("2013-07-01"),
         Date <= ymd("2025-06-30"))
cat("After date filter:", nrow(dat), "\n")

# -- Six-book odds columns --------------------------------------------------
book_cols <- c("B365H","B365D","B365A",
               "BWH","BWD","BWA",
               "IWH","IWD","IWA",
               "WHH","WHD","WHA",
               "VCH","VCD","VCA",
               "PSCH","PSCD","PSCA")

# All odds must be present and positive
dat <- dat %>%
  filter(if_all(all_of(book_cols), ~ !is.na(.) & . > 1)) %>%
  filter(!is.na(FTR), FTR %in% c("H","D","A"))
cat("After complete-six-book + FTR filter:", nrow(dat), "\n")

# -- Season and tier --------------------------------------------------------
top_tier <- c("E0","D1","I1","SP1","F1")
dat <- dat %>%
  mutate(
    season_start = if_else(month(Date) >= 7, year(Date), year(Date) - 1L),
    Season = paste0(season_start, "/", substr(as.character(season_start + 1L), 3, 4)),
    Tier   = if_else(Div %in% top_tier, "Top", "Second")
  )

# -- Implied probabilities per book and de-vigged (naive normalization) ----
books <- list(
  B365 = c("B365H","B365D","B365A"),
  BW   = c("BWH","BWD","BWA"),
  IW   = c("IWH","IWD","IWA"),
  WH   = c("WHH","WHD","WHA"),
  VC   = c("VCH","VCD","VCA"),
  PSC  = c("PSCH","PSCD","PSCA")
)

for (bk in names(books)) {
  cols <- books[[bk]]
  oH <- 1 / dat[[cols[1]]]
  oD <- 1 / dat[[cols[2]]]
  oA <- 1 / dat[[cols[3]]]
  S  <- oH + oD + oA
  dat[[paste0("S_",   bk)]] <- S
  dat[[paste0("qH_",  bk)]] <- oH / S
  dat[[paste0("qD_",  bk)]] <- oD / S
  dat[[paste0("qA_",  bk)]] <- oA / S
}

# Aggregate across six books
S_mat  <- as.matrix(dat[, paste0("S_",  names(books))])
qH_mat <- as.matrix(dat[, paste0("qH_", names(books))])
qD_mat <- as.matrix(dat[, paste0("qD_", names(books))])
qA_mat <- as.matrix(dat[, paste0("qA_", names(books))])

dat$overround_mean <- rowMeans(S_mat)
dat$q_H_mean <- rowMeans(qH_mat)
dat$q_D_mean <- rowMeans(qD_mat)
dat$q_A_mean <- rowMeans(qA_mat)
dat$q_H_sd   <- apply(qH_mat, 1, sd)
dat$q_D_sd   <- apply(qD_mat, 1, sd)
dat$q_A_sd   <- apply(qA_mat, 1, sd)

# Favourite / longshot using H and A only (not D)
dat$p_fav  <- pmax(dat$q_H_mean, dat$q_A_mean)
dat$p_long <- pmin(dat$q_H_mean, dat$q_A_mean)

# -- Save -------------------------------------------------------------------
saveRDS(dat, file.path(data_dir, "analysis_sample.rds"))
write_csv(dat, file.path(data_dir, "analysis_sample.csv"))

cat("\n==== FINAL SAMPLE ====\n")
cat("n matches:", nrow(dat), "\n")
cat("n leagues:", n_distinct(dat$Div), "\n")
cat("date range:", as.character(min(dat$Date)), "to", as.character(max(dat$Date)), "\n")
cat("seasons:",   paste(sort(unique(dat$Season)), collapse=", "), "\n")
cat("tier counts:\n"); print(table(dat$Tier))
cat("overround_mean summary:\n"); print(summary(dat$overround_mean))
