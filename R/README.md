# FLB_Final_Code — Reproducibility Archive

Consolidated codebase for the STAT 441 term paper on the favourite–longshot
bias (FLB) in European football betting markets. This archive reproduces
every figure and table in the PDF report from a single raw-data dependency.

## Reproduction

1. Place the raw CSV at `~/Desktop/full_dataset.csv` (Football-Data.co.uk
   scrape, July 2005 – April 2026; 68,359 matches × 198 columns).
2. Install R packages:
   ```r
   source("~/Desktop/FLB_Final_Code/requirements.R")
   ```
3. Run the R scripts in numeric order:
   ```bash
   cd ~/Desktop/FLB_Final_Code
   for s in scripts/0*.R scripts/1*.R; do Rscript "$s"; done
   ```
   Each script is self-contained: it reads only from `data/` (produced by
   earlier scripts) and writes to `data/`, `figures/`, `tables/`, or
   `models/`.
4. Render the three styled table PNGs (Latin Modern Roman):
   ```bash
   cd ~/Desktop/FLB_Final_Code/scripts/table_renderers
   python3 render_table_2_1_sample_by_league.py
   python3 render_table_4_7_per_regime.py
   python3 render_table_5_2_regime_strategies.py
   ```

Random seeds (`set.seed(441)`) are preserved everywhere, so outputs are
bit-for-bit identical to the PDF run.

## Folder map

```
FLB_Final_Code/
├── README.md                       # this file
├── requirements.R                  # one-line package installer
├── data/                           # analysis_sample.rds, features, predictions
├── models/                         # elasticnet_fit.rds, rf_fit.rds, xgb_fit.rds
├── figures/                        # PNG 300 dpi, matches the PDF
├── tables/                         # CSV + LaTeX (plus PNG for the styled three)
└── scripts/
    ├── 00_setup.R                  # filter raw CSV -> analysis_sample.rds
    ├── 01_eda.R                    # Section 2 EDA (Figures 1-3, Section 2.4)
    ├── 02_regime_features.R        # 9 market-shape features
    ├── 03_pca.R                    # Figure 4 (scree) + loading table
    ├── 04_kmeans.R                 # Figure 5 (K selection) + cluster profiles
    ├── 05_hclust.R                 # Figure 6 (dendrogram) + ARI
    ├── 06_regime_flb.R             # Figure 7 (reliability per regime)
    ├── 07_teamform.R               # 28 team-form features + feature matrix
    ├── 08_baselines.R              # Naive + Shin de-vigging baselines
    ├── 09_elasticnet.R             # cv.glmnet Elastic Net
    ├── 10_randomforest.R           # ranger RF + Figure 8 (importance)
    ├── 11_xgboost.R                # xgb.cv + xgb.train + importance
    ├── 12_per_regime_eval.R        # Figure 9 (Table 4.7)
    ├── 13_betting_strategies.R     # Bet ledgers + unconditional ROI
    ├── 14_pnl_and_tables.R         # Figure 10 (Table 5.2) + Figure 11 (P&L)
    ├── 15_sensitivity.R            # Threshold / odds / season / Kelly
    └── table_renderers/
        ├── _style.py               # shared colours + fonts
        ├── render_table_2_1_sample_by_league.py
        ├── render_table_4_7_per_regime.py
        └── render_table_5_2_regime_strategies.py
```

## Script → report figure / table map

| Script | Produces in the report |
|---|---|
| `00_setup.R`                | Base dataset `analysis_sample.rds` (no PDF output) |
| `01_eda.R`                  | Figure 1 = Table 2.1 (sample by league); Figure 2 (overround histogram + by-league boxplot); Figure 3 (pooled reliability); Section 2.4 faceted reliability (tier, overround quartile, disagreement quartile); Section 2.5 hex-bin disagreement |
| `02_regime_features.R`      | 9 market-shape features stored under `data/` |
| `03_pca.R`                  | Figure 4 (scree) + Table 3.1 (PC loadings) |
| `04_kmeans.R`               | Figure 5 (K-selection 3-panel) + Table 3.2 (cluster profiles) + `data/kmeans_fit.rds` |
| `05_hclust.R`               | Figure 6 (dendrogram) + Table 3.3 (K-Means vs Hclust confusion) + ARI |
| `06_regime_flb.R`           | Figure 7 (reliability per regime) + Table 3.4 / 3.5 (regime summary) |
| `07_teamform.R`             | 28 rolling team-form features + standardised train/test feature matrices |
| `08_baselines.R`            | Table 4.2 (Naive vs Shin); stores `baseline_preds_test.rds` |
| `09_elasticnet.R`           | Table 4.3 (coefs); stores `elasticnet_preds_test.rds` |
| `10_randomforest.R`         | Figure 8 (RF importance) + Table 4.4; stores `rf_preds_test.rds` |
| `11_xgboost.R`              | XGB importance figure + Table 4.5; stores `xgb_preds_test.rds` |
| `12_per_regime_eval.R`      | Figure 9 (per-regime Brier bars) + Table 4.6 / 4.7 / 4.8 |
| `13_betting_strategies.R`   | Bet ledgers (`bets_{shin,en,xgb}.rds`) + Table 5.1 (unconditional) |
| `14_pnl_and_tables.R`       | Figure 10 = Table 5.2 (regime strategies) + Figure 11 (cumulative P&L) + Table 5.6 (headline) |
| `15_sensitivity.R`          | Tables 5.3 / 5.4 / 5.5 (EV-threshold, odds-source, per-season) + Table 5.7 (Kelly) + Figure 5.2 + Figure 5.3 |
| `table_renderers/render_table_2_1_*.py`  | Figure 1 (styled PNG of Table 2.1) |
| `table_renderers/render_table_4_7_*.py`  | Figure 9 companion (styled PNG of Table 4.7) |
| `table_renderers/render_table_5_2_*.py`  | Figure 10 companion (styled PNG of Table 5.2) |

## Data source

Football-Data.co.uk (https://www.football-data.co.uk/), six-bookmaker 1X2
odds for 11 European leagues (E0, E1, E2, D1, D2, I1, I2, SP1, SP2, F1, F2),
seasons 2013/14 through 2024/25, downloaded as `full_dataset.csv`.

## Conventions preserved from the original section-by-section build

- **Train / test split**: 2013/14 – 2020/21 train, 2021/22 – 2024/25 test.
- **Strict anti-leakage for team form**: every rolling statistic at match
  M uses only matches strictly before M's date for that team. Verified by
  three sanity checks in `07_teamform.R`.
- **Train-only fitting**: PCA centre/scale, K-Means centroids, and
  Ward-linkage dendrogram are fit only on the training window; test
  matches are assigned to regimes by projection + nearest-centroid
  snapping.
- **PNG only, 300 dpi** — no PDF figures.
- **Settlement odds for Section 5** = Pinnacle closing (`PSCH/PSCD/PSCA`);
  max-of-six and mean-of-six are sensitivity checks only.
- **Stake**: 1 unit flat in the main analysis; quarter-Kelly robustness in
  `15_sensitivity.R`.
