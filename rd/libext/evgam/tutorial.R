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

df_train[, y          := ys$excursion_high[train_indices]]
df_train[, y_kurtosis := ys$kurtosis[train_indices]]
df_train[, y_skewness := ys$skewness[train_indices]]


# Convert to data.frame for modeling
df_train <- df_train %>% as.data.frame()

# Remove rows where ANY of the y-variables are NA
df_train <- df_train %>% drop_na(y, y_kurtosis, y_skewness)

cat("Rows after cleaning target columns:", nrow(df_train), "\n")



###########################################################
# 1. Choose predictors to test (simple regression)
###########################################################

basic_predictors <- setdiff(
  colnames(df_train),
  c(
    "y", "excess",
    "smoothed_close", "smoothed_close_1", "smoothed_close_2",
    "rsi_2"
    )
)[1:30]
basic_predictors <- c(
  "vol", "vol_1", "vol_2", "vix",
  "y_skewness", "y_kurtosis",
  "H", "H_slow",
  "ae_recon_error", "ae_volatility",
  "R_1"
)

evt_formula <- as.formula(
  paste("excess ~", paste(basic_predictors, collapse = " + "))
)
evt_formula


###########################################################
# 2. Build exceedance dataset
###########################################################

u <- 1.30

df_ext <- df_train %>%
  filter(y > u) %>%
  mutate(excess = y - u)

cat("Exceedances:", nrow(df_ext), "\n")

df_ext_mod <- df_ext %>%
  select(excess, all_of(basic_predictors)) %>%
  drop_na()

cat("Rows in df_ext_mod:", nrow(df_ext_mod), "\n")


###########################################################
# 3. EVT formula
###########################################################

evt_formula <- as.formula(
  paste("excess ~", paste(basic_predictors, collapse = " + "))
)

cat("Using formula:\n")
print(evt_formula)


###########################################################
# 4. Fit EVT
###########################################################

fit_evt <- evgam(
  formula = evt_formula,
  family  = "gpd",
  data    = df_ext_mod
)

summary(fit_evt)
