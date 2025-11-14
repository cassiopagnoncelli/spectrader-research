###########################################################
# OPTION C — FULL EVT PIPELINE
# Tail modeling for y using evgam::evgam + XGBoost feature selection
###########################################################

suppressPackageStartupMessages({
  library(evgam)
  library(dplyr)
  library(ggplot2)
  library(Matrix)
  library(xgboost)
  library(tidyr)
})

###########################################################
# 0. BUILD df_train
###########################################################

# X is your data.table with features
# train_indices is a vector of indices
# ys contains your targets

df_train <- X[train_indices]   # <-- DO NOT TOUCH X
setDT(df_train)                # <-- make df_train a data.table

df_train[, y          := ys$extreme_high_identity[train_indices]]
df_train[, y_kurtosis := ys$kurtosis[train_indices]]
df_train[, y_skewness := ys$skewness[train_indices]]


# Convert to data.frame for modeling
df_train <- df_train %>% as.data.frame()

# Remove rows where ANY of the y-variables are NA
df_train <- df_train %>% drop_na(y, y_kurtosis, y_skewness)

cat("Rows after cleaning target columns:", nrow(df_train), "\n")


###########################################################
# 1. DEFINE EVT THRESHOLD u
###########################################################

u <- 1.30
df_train$y_bin <- as.integer(df_train$y > u)

cat("Threshold u =", u, "\n")


###########################################################
# 2. NA CLEANUP FOR FEATURE SELECTION
###########################################################

# Drop all remaining NA rows globally before XGBoost
df_xgb <- df_train %>% drop_na()

cat("Rows after global NA drop:", nrow(df_xgb), "\n")


###########################################################
# 3. XGBoost FEATURE SELECTION
###########################################################

# Build sparse matrix for XGBoost
Xmat <- sparse.model.matrix(
  ~ . - y - y_bin,
  data = df_xgb
)[, -1]   # remove intercept

dtrain <- xgb.DMatrix(
  data  = Xmat,
  label = df_xgb$y_bin
)

cat("Matrix rows:", nrow(Xmat), "Labels:", length(df_xgb$y_bin), "\n")
stopifnot(nrow(Xmat) == length(df_xgb$y_bin))

params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 6,
  eta = 0.05,
  subsample = 0.9,
  colsample_bytree = 0.8
)

cat("\nTraining XGBoost for EVT feature selection...\n")

xgb_fit <- xgb.train(
  params = params,
  data   = dtrain,
  nrounds = 300,
  verbose = 0
)

importance <- xgb.importance(model = xgb_fit)

cat("\nTop 20 features:\n")
print(head(importance, 20))

# TOP-K predictors for σ(Z)
k <- 40
top_sigma_features <- importance$Feature[1:k]

cat("\nSelected σ(Z) predictors:\n")
print(top_sigma_features)


###########################################################
# 4. STRUCTURAL FEATURES FOR ξ(Z)
###########################################################

predictors_shape <- intersect(
  c(
    "y_skewness", "y_kurtosis",
    "vol", "vol_1", "vol_2",
    "vix", "vix_1", "vix_2",
    "H", "H_slow", "H_1",
    "ae_recon_error", "ae_volatility"
  ),
  names(df_train)
)

cat("\nSelected ξ(Z) predictors:\n")
print(predictors_shape)


###########################################################
# 5. BUILD EXCEEDANCE DATASET
###########################################################

df_ext <- df_train %>%
  filter(y > u) %>%
  mutate(excess = y - u)

cat("Exceedances:", nrow(df_ext), "\n")

df_ext_mod <- df_ext %>%
  select(excess, all_of(top_sigma_features), all_of(predictors_shape)) %>%
  drop_na()

cat("Rows in EVT model:", nrow(df_ext_mod), "\n")


###########################################################
# 6. BUILD FORMULAS FOR EVT GPD REGRESSION
###########################################################

sigma_formula <- as.formula(
  paste("excess ~", paste(top_sigma_features, collapse = " + "))
)

shape_formula <- as.formula(
  paste("~", paste(predictors_shape, collapse = " + "))
)

cat("\nσ(Z) formula:\n")
print(sigma_formula)
cat("\nξ(Z) formula:\n")
print(shape_formula)


###########################################################
# 7. FIT EVT MODEL
###########################################################

cat("\n=== Fitting EVT GPD Regression ===\n")

fit_evt <- evgam(
  formula = sigma_formula,
  shape   = shape_formula,
  family  = "gpd",
  data    = df_ext_mod
)

cat("\n===== EVT MODEL SUMMARY =====\n")
print(summary(fit_evt))


###########################################################
# 8. PARAMETER EXTRACTION FUNCTIONS
###########################################################

evt_params <- function(newdata, fit = fit_evt) {
  logscale_hat <- predict(fit, newdata, type = "link", what = "scale")
  shape_hat    <- predict(fit, newdata, type = "link", what = "shape")

  tibble(
    sigma = exp(logscale_hat),
    xi    = shape_hat
  )
}


###########################################################
# 9. TAIL PROBABILITY AND QUANTILE FUNCTIONS
###########################################################

gpd_survival <- function(y_star, u, sigma, xi) {
  z <- y_star - u
  z[z < 0] <- 0
  arg <- 1 + xi * (z / sigma)
  arg[arg <= 0] <- NA_real_
  arg^(-1/xi)
}

gpd_quantile <- function(p, u, sigma, xi) {
  z <- (sigma / xi) * (p^(-xi) - 1)
  u + z
}


###########################################################
# 10. EXAMPLE PREDICTIONS
###########################################################

cat("\nPredicting parameters for first 10 exceedances...\n")
params_example <- evt_params(df_ext_mod[1:10, ])
print(params_example)

for (ys in c(1.40, 1.50)) {
  cat("\nP(y >", ys, "| y > u, Z) for first 10 rows:\n")
  print(gpd_survival(ys, u, params_example$sigma, params_example$xi))
}

for (p in c(0.99, 0.995)) {
  cat("\nTail quantile Q(", p, "| y > u, Z) for first 10 rows:\n")
  print(gpd_quantile(p, u, params_example$sigma, params_example$xi))
}

cat("\n===== EVT PIPELINE COMPLETED SUCCESSFULLY =====\n")
