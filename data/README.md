# Data

`full_dataset.csv` (~33.6 MB, 68,359 rows × 198 columns) is **not committed** to this repo
because of size and Football-Data.co.uk's redistribution terms. Fetch it yourself:

1. Go to [https://www.football-data.co.uk/data.php](https://www.football-data.co.uk/data.php).
2. Download the per-season CSVs for the 11 leagues used in this project, seasons 2005/06 – 2025/26.
3. Concatenate into `data/raw/full_dataset.csv` (union of columns).
4. `Rscript R/01_preprocess.R` will write `data/processed/` (also gitignored).

## Pre-match feature whitelist (leakage guard)

Only columns in the following categories are allowed into predictive models:

- **Identifiers:** `Div`, `Date`, `HomeTeam`, `AwayTeam`, `Season`
- **Target:** `FTR` (only for y)
- **1X2 odds:** `B365H/D/A`, `BWH/D/A`, `IWH/D/A`, `WHH/D/A`, `VCH/D/A`, `PSH/D/A`, etc.
- **Derived pre-match features:** implied probs, overround, cross-book dispersion,
  rolling team form (computed from past matches only)

Explicitly **excluded** (post-match, would leak):
`FTHG, FTAG, HTHG, HTAG, HTR, HS, AS, HST, AST, HF, AF, HC, AC, HY, AY, HR, AR`.

See `R/01_preprocess.R::PREMATCH_WHITELIST` for the canonical list.
