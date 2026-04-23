# requirements.R - install every R package touched by the FLB_Final_Code pipeline.
# Run once before executing scripts/00_setup.R through scripts/15_sensitivity.R.

required_packages <- c(
  # Core
  "tidyverse", "lubridate", "patchwork",
  # ML
  "glmnet", "ranger", "xgboost",
  # Clustering / multivariate
  "cluster", "fpc", "mclust",
  # Rolling windows + styled tables
  "slider", "kableExtra",
  # Plot helpers / model diagnostics
  "hexbin", "pROC", "scales", "gridExtra",
  # Listed for completeness (some scripts import but do not strictly require)
  "MASS", "nnet"
)

installed <- rownames(installed.packages())
new <- required_packages[!(required_packages %in% installed)]

if (length(new)) {
  install.packages(new, repos = "https://cloud.r-project.org")
}
cat("All required R packages installed.\n")
