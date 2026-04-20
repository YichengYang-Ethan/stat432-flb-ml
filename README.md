# STAT432 Project — Machine Learning for the Favourite–Longshot Bias (FLB)

Can parametric and non-parametric ML methods identify and exploit the Favourite–Longshot
Bias (FLB) in bookmaker odds, and can they outperform traditional implied-probability
baselines (Shin, Power, basic normalization)?

**Course:** STAT 432, Spring 2026, UIUC
**Author:** Yicheng (Ethan) Yang

## Research Questions

1. **Unsupervised structure.** Can PCA / K-means on cross-bookmaker odds reveal latent
   market regimes (heavy favorite, balanced, longshot), and does FLB severity differ
   across regimes?
2. **Predictive modeling (core).** Can parametric and non-parametric ML methods
   (Multinomial Logistic + Lasso/Ridge, LDA, QDA, KNN, Random Forest, XGBoost, SVM-RBF)
   identify FLB from pre-match features and outperform Shin / Power baselines on
   log-loss and calibration?
3. **Economic value.** Does a betting strategy constructed from ML-based probabilities
   achieve positive expected return out of sample (flat stake + fractional Kelly),
   and does the edge survive a closing-line-value (CLV) check?

## Dataset

- Source: [Football-Data.co.uk](https://www.football-data.co.uk/)
- 68,359 matches from 11 European leagues, July 2005 – April 2026
- 198 columns: full-time result (FTR), match statistics, 1X2 odds from multiple books
- Primary odds source: Bet365 (B365H/D/A, 0.1% missing)
- **Leakage guard:** in-game statistics (shots, corners, cards, HT score) are available
  only post-match and are excluded from all predictive models via an explicit
  **pre-match feature whitelist**.

Data is **not** committed to the repo (file size + redistribution). See
[`data/README.md`](data/README.md) for how to fetch `full_dataset.csv`.

## Repository Layout

```
stat432-flb-ml/
├── data/              # raw + processed data (gitignored)
├── notebooks/         # exploratory + narrative notebooks (.Rmd / .ipynb)
├── R/                 # R source (preprocessing, baselines, ML)
├── src/               # Python source (optional, for XGBoost/SVM if needed)
├── results/           # figures, tables, metrics (gitignored except key outputs)
├── report/            # final write-up (.Rmd / .tex / .pdf)
└── tests/             # unit tests for preprocessing + leakage guard
```

## Analysis Plan

| Stage | Deliverable |
|------|-------------|
| 1. Preprocessing | Clean, engineer pre-match features (implied probs, overround, cross-book dispersion, rolling form) |
| 2. Unsupervised | PCA on 60+ odds cols, K-means regime clustering |
| 3. Baselines | Basic normalization, Shin, Power implied probabilities |
| 4. Classification | Multinomial Logistic (Lasso/Ridge), LDA, QDA, KNN, RF, XGBoost, SVM-RBF |
| 5. FLB confirmation | Cain–Law–Peel (2003) log-odds regression (β < 1 ⇒ FLB) |
| 6. Evaluation | Log-loss, accuracy, **reliability diagrams per class**, Diebold–Mariano test |
| 7. Economic value | Flat-stake + fractional Kelly; realized ROI + CLV on held-out seasons |
| 8. Robustness | Cross-league, cross-era stability; feature importance |

All train/test splits are **strictly chronological** to prevent information leakage.

## Reproducing

```bash
# 1. fetch data into data/raw/ (see data/README.md)
# 2. run preprocessing
Rscript R/01_preprocess.R
# 3. run baselines + models
Rscript R/02_baselines.R
Rscript R/03_ml_models.R
# 4. reproduce report
Rscript -e "rmarkdown::render('report/report.Rmd')"
```

## References

- Cain, M., Law, D., & Peel, D. (2003). *The favourite-longshot bias, bookmaker margins and insider trading.* Applied Economics.
- Shin, H. S. (1993). *Measuring the incidence of insider trading in a market for state-contingent claims.* Economic Journal.
- Štrumbelj, E. (2014). *On determining probability forecasts from betting odds.* International Journal of Forecasting.

## License

MIT — see [LICENSE](LICENSE).
