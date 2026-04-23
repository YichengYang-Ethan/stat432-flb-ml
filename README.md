# STAT432 Project — Machine Learning for the Favourite–Longshot Bias (FLB)

Does the favourite–longshot bias — bettors systematically overvaluing longshots
and undervaluing favourites — actually show up in modern European football
markets, and can machine learning turn it into money?

**Course:** STAT 432, Spring 2026, UIUC
**Author:** Yicheng (Ethan) Yang

## Status

Pipeline is done. Drop the raw CSV in place, run the 16 R scripts in order,
and you get every number and figure in the report. Write-up is in progress
under `report/`.

## What we asked and what we found

**1. Are there different kinds of betting markets?** Yes.
We built 9 features describing what the six bookmakers are quoting on each
match, boiled them down to 3 PCA components (87.4 % of the variance), and
clustered the matches into **4 groups** with K-Means. One group has the books
disagreeing a lot with each other (we call it the Wild West); one is dominated
by big home favourites; one leans towards away wins; the last is moderately
home-leaning. The favourite–longshot bias shows up with different intensity
in each group — the regression slope of realised frequency on implied
probability runs from **1.07 in the calmest regime to 1.15 in the Wild West**.

**2. Can ML beat the bookmaker's own de-vigged probability?** Barely, and
only in one regime. On the pooled test set (2021/22 – 2024/25, ~10,000
matches) every model lands on a Brier score between 0.600 and 0.608 —
essentially a tie. The Shin baseline wins overall sharpness; Random Forest
wins calibration. **On the Wild West regime only**, XGBoost and Elastic Net
slip under both baselines on Brier by 0.1–0.2 percentage points. Outside that
regime no ML model beats the bookmaker.

**3. Would any of this make money?** No, not at Pinnacle closing prices.
Every one of our 12 (model × strategy) combinations lost money over the test
window. The least-bad cell is Elastic Net betting only on Regimes 1+2,
ROI = **−3.1 %**. Quarter-Kelly sizing reduces the bleed uniformly (every
cell improves by about +1.5 pp) but not a single one flips into the black.
The 2021/22 season was briefly profitable (ROI +1 to +3 % depending on
model), but the edge vanishes in 2022/23 and 2023/24.

## Three figures that tell the story

### FLB is real in the pooled sample

![Pooled reliability diagram](results/figure_2_3_reliability_pooled.png)

*For each bin of implied probability (x), we plot the fraction of matches that
actually happened (y). Perfect bookmaking would sit on the dashed 45° line.
Our 20 bins lie above the line for high probabilities and below for low ones —
the textbook FLB pattern. A weighted regression gives slope 1.079.*

### ML only beats the bookmaker in the Wild West

![Per-regime Brier](results/figure_4_4_per_regime_brier.png)

*Brier score per regime (lower is better). The dashed segment in each regime
is the naive-baseline Brier. The green XGBoost bar dips under that line only
in Regime 1. Regime 2 has very low Brier because strong home favourites are
easy to forecast — but no ML model helps there either.*

### Nothing actually makes money at Pinnacle closing

![Cumulative P&L](results/figure_5_1_cumulative_pnl.png)

*Cumulative profit in units of stake, one panel per model, 1-unit flat
stakes, 2021/22 – 2023/24. Every strategy trends downward. The green line
(bet only on Regimes 1+2) stays closest to zero — it bleeds about half as
fast as the rest — but still bleeds. The red control line (bet only on
Regimes 3+4) and the grey unconditional line behave similarly, which is what
we expected.*

## The data

- **Source:** [Football-Data.co.uk](https://www.football-data.co.uk/)
- **Raw:** 68,359 matches, 11 European leagues (Premier League, Championship,
  League One, both Bundesligas, both Serie A/B, La Liga 1/2, Ligue 1/2),
  July 2005 – April 2026, 198 columns
- **After filtering** to matches with complete 1X2 odds from all six target
  bookmakers (Bet365, Bwin, Interwetten, William Hill, VC Bet, Pinnacle
  closing) in the 2013/14 – 2024/25 window → **42,536 matches**
- **After requiring 10 prior matches of team history** for rolling form →
  **36,220 matches** (train = 26,153 in 2013/14–2020/21, test = 10,067 in
  2021/22–2024/25)
- **No data leakage:** post-match stats (shots, corners, cards, goals,
  half-time score) are never used as features. The one exception is rolling
  historical averages of past shots-on-target, which are allowed because they
  only look at earlier matches.

Raw data is not committed to the repo. See
[`data/README.md`](data/README.md) for how to download `full_dataset.csv`.

## The 16 scripts

Each script is self-contained: it reads from `data/` (built by an earlier
script) and writes to `data/`, `results/`, or `tables/`. Train / test splits
are strictly chronological. Every random seed is fixed at `set.seed(441)` so
reruns match the report exactly.

| # | Script | What it does |
|---|---|---|
| 00 | `R/00_setup.R` | Filter raw CSV, build the base sample |
| 01 | `R/01_eda.R` | Exploratory plots: overround, pooled reliability, heterogeneity, cross-book disagreement |
| 02 | `R/02_regime_features.R` | 9 market-shape features (p_fav, entropy, cross-book sd, …) |
| 03 | `R/03_pca.R` | PCA on the training window |
| 04 | `R/04_kmeans.R` | Pick K by silhouette, fit the K-Means model |
| 05 | `R/05_hclust.R` | Ward-linkage hierarchical clustering, compare to K-Means (ARI) |
| 06 | `R/06_regime_flb.R` | One reliability diagram per regime |
| 07 | `R/07_teamform.R` | 28 rolling team-form features with three anti-leakage sanity checks |
| 08 | `R/08_baselines.R` | Naive and Shin de-vigging baselines |
| 09 | `R/09_elasticnet.R` | `cv.glmnet` multinomial Elastic Net |
| 10 | `R/10_randomforest.R` | `ranger` probability forest + importance plot |
| 11 | `R/11_xgboost.R` | `xgb.cv` picks `nrounds`, then `xgb.train` fits |
| 12 | `R/12_per_regime_eval.R` | Overall and per-regime evaluation |
| 13 | `R/13_betting_strategies.R` | Build bet ledgers, compute unconditional ROI |
| 14 | `R/14_pnl_and_tables.R` | Regime-conditional strategies + cumulative P&L figure |
| 15 | `R/15_sensitivity.R` | EV threshold, odds source, per-season, and quarter-Kelly checks |

## Folder layout

```
stat432-flb-ml/
├── data/              # raw + processed data (gitignored)
├── notebooks/         # exploratory + narrative notebooks (.Rmd / .ipynb)
├── R/                 # the 16 scripts + table_renderers/ + README
├── src/               # Python source (unused so far)
├── results/           # key figures committed; .rds / .pkl gitignored
├── report/            # final write-up (.Rmd / .tex / .pdf)
├── tests/             # unit tests for preprocessing + leakage guard
├── requirements.R     # one-line installer for every R package used
└── README.md
```

## How to run it

```bash
# 1. Get the CSV into data/ (see data/README.md)
# 2. Install R packages
Rscript requirements.R

# 3. Run the pipeline
cd R && for s in *.R; do Rscript "$s"; done

# 4. Render the styled table PNGs (optional, needs Python + matplotlib)
cd R/table_renderers
python3 render_table_2_1_sample_by_league.py
python3 render_table_4_7_per_regime.py
python3 render_table_5_2_regime_strategies.py
```

## References

- Cain, M., Law, D., & Peel, D. (2003). *The favourite–longshot bias, bookmaker margins and insider trading.* Applied Economics.
- Shin, H. S. (1993). *Measuring the incidence of insider trading in a market for state-contingent claims.* Economic Journal.
- Štrumbelj, E. (2014). *On determining probability forecasts from betting odds.* International Journal of Forecasting.

## License

MIT — see [LICENSE](LICENSE).
