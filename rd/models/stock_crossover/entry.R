devtools::load_all()

options(scipen = 999)

source("rd/models/stock_crossover/entry_model.R")
source("rd/models/stock_crossover/entry_plots.R")

#
# ETL.
#
fetl <- fets::Fetl$new()

vix <- fets::get_vix(fetl)
quotes <- fets::get_quotes(fetl)
fets::add_vix(quotes, vix)

#
# FEATURE ENGINEERING.
#
fets::fwd(quotes, lookahead = 15, inplace = TRUE)
Xyfe <- fets::fe(quotes, inplace = TRUE)
Xy <- Xyfe$X

Xym <- fets::decomposeXy(Xy, na.rm = TRUE)
X <- Xym$X
ys <- Xym$y
meta <- Xym$meta
metaX <- cbind(meta, X)

# Re-engineer features, drilling down to the most important ones
if (TRUE) {
  keep_features <- c(
    names(X)[grepl("^y", names(X))],
    "vix_2", "wh", "wh_2", "vix_1", "vix", "vix_vel_0", "wh_vel_1", "wh_1",
    "vix_vel_1", "H_slow", "vix_accel_0", "wh_vel_0", "vol_vix_2", "wh_accel_0",
    "signal_fast_ratio_2", "H_slow_2", "ae_recon_error_2", "macro", "vol_1",
    "vol_vix_vel_0", "ae_recon_error_1", "vol_vix", "slow_2", "ae_volatility_vel_0",
    "fast_1", "fast_slow_ratio_vel_1", "skewness", "ae_volatility", "cr_7",
    "slow_1", "R_2", "slow", "vol_vix_vel_1", "H_slow_vel_1"
  )
  remove_features <- setdiff(Xyfe$new_features, keep_features)
  X[, (remove_features) := NULL]
}

#
# PREPROCESSING.
#

# X[, .(close) := NULL]

# Set splits.
train_end <- as.Date("2023-12-31")
val_end <- as.Date("2024-10-31")

train_indices <- which(meta$date <= train_end)
train_indices <- sample(train_indices, 35000) # Train limit for faster training
val_indices <- which(meta$date > train_end & meta$date <= val_end)
test_indices <- which(meta$date > val_end)

train_data <- X[train_indices, ]
val_data <- X[val_indices, ]
test_data <- X[test_indices, ]

#
# Training
#
fets::fwd_methods()
aux_list <- list(
  # High/Low
  y1 = ys$extreme_high_identity,
  y2 = ys$extreme_low_identity,
  y3 = ys$mass_high,
  y4 = ys$mass_low,
  # Sharpe
  ys2 = ys$dd_sharpe,
  ys3 = ys$entropy_sharpe,
  # Differentials
  yd1 = ys$de,
  yd2 = ys$dm,
  # Moments
  # ym1 = ys$skewness,
  # Moving Averages
  yma1 = ys$ma_short_ratio,
  yma2 = ys$ma_long_ratio,
  yma3 = ys$ma_macro_ratio,
  # Close
  yc = ys$close_identity
)
aux <- aux_list[setdiff(names(aux_list), c())]
model_signal <- train_stacked_model(
  train_indices = train_indices,
  val_indices = val_indices,
  test_indices = test_indices,
  X = X,
  y = ys$skewness,
  aux = list(),
  verbose = TRUE
)

feature_importance <- tibble(model_signal$importance) %>% print(n = Inf)

#
# EVALUATION.
#
df_train <- tibble(
  symbol = meta$symbol[train_indices],
  date = meta$date[train_indices],
  y = model_signal$actuals$train,
  yhat = model_signal$predictions$train,
  close = ys$close_log[train_indices]
)

df_val <- tibble(
  symbol = meta$symbol[val_indices],
  date = meta$date[val_indices],
  y = model_signal$actuals$val,
  yhat = model_signal$predictions$val,
  close = ys$close_log[val_indices]
)

df_test <- tibble(
  symbol = meta$symbol[test_indices],
  date = meta$date[test_indices],
  y = model_signal$actuals$test,
  yhat = model_signal$predictions$test,
  close = ys$close_log[test_indices]
)
