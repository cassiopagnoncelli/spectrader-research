devtools::load_all()

options(scipen = 999)

source("rd/models/stock_crossover/entry_model.R")
source("rd/models/stock_crossover/entry_plots.R")

#
# ETL.
#
fetl <- fets::Fetl$new()

vix <- fets::get_vix(fetl)
q <- fets::get_quotes(fetl)

#
# FEATURE ENGINEERING.
#
fets::fwd(q, inplace = TRUE, lookahead = 20)

fets::add_vix(q, vix)

qfe <- fets::fe(q, inplace = FALSE) %>% na.omit
q <- qfe$dt

# Re-engineer features, drilling down to the most important ones
keep_features <- c(
  "yt2_pred", "yt3_pred","y2_pred","vix_2","wh",
  "wh_2","vix_1","vix","vix_vel_0","y1_pred",
  "wh_vel_1","wh_1","vix_vel_1","H_slow","vix_accel_0",
  "wh_vel_0","vol_vix_2","y3_pred","wh_accel_0","y4_pred",
  "signal_fast_ratio_2","H_slow_2","ae_recon_error_2","macro","vol_1",
  "vol_vix_vel_0","ae_recon_error_1","vol_vix","slow_2","ae_volatility_vel_0",
  "fast_1","fast_slow_ratio_vel_1","skewness","ae_volatility","cr_7",
  "slow_1","R_2","slow","vol_vix_vel_1","H_slow_vel_1"
)
remove_features <- setdiff(qfe$new_features, keep_features)
q[, (remove_features) := NULL]

#
# PREPROCESSING.
#
q_metadata <- q[, .(symbol, date)]
q_targets <- q[, .SD, .SDcols = fets::fwd_methods()]
q_X <- q[, .SD, .SDcols = !c("symbol", "date", "close", fets::fwd_methods())]

# Set splits.
train_end <- as.Date("2023-12-31")
val_end <- as.Date("2024-10-31")

train_indices <- which(q$date <= train_end)
train_indices <- sample(train_indices, 35000) # Train limit for faster training
val_indices <- which(q$date > train_end & q$date <= val_end)
test_indices <- which(q$date > val_end)

train_data <- q_X[train_indices, ]
val_data <- q_X[val_indices, ]
test_data <- q_X[test_indices, ]

#
# Training
#
model_name <- "stock_crossover__entry__extreme_high_identity"
params <- list(rounds = 200, notes = "neutral")
ckm <- cache_key(params = params, ext = "rds", fun = model_name)
aux_list <- list(
  y1 = q_targets$extreme_high_identity,
  y2 = q_targets$extreme_low_identity,
  y3 = q_targets$mass_high_identity,
  y4 = q_targets$mass_low_identity,
  y5 = q_targets$sharpe_high,
  y6 = q_targets$sharpe_low,
  y7 = q_targets$close_log,
  y8 = q_targets$mean_log,
  y9 = q_targets$de_log,
  y10 = q_targets$dm_log,
  yt1 = q_targets$ma_short_ratio,
  yt2 = q_targets$ma_long_ratio,
  yt3 = q_targets$ma_macro_ratio
)
aux <- aux_list[c("y1", "y2", "y3", "y4", "yt2", "yt3")]
aux <- names(aux_list)
model_signal <- train_stacked_model(
  train_indices = train_indices,
  val_indices = val_indices,
  test_indices = test_indices,
  X = q_X,
  y = q_targets$extreme_high_identity,
  aux = aux,
  verbose = TRUE
)

feature_importance <- model_signal$importance %>% as_tibble() %>% print(n = Inf)

#
# EVALUATION.
#
df_train <- tibble(
  symbol = q_metadata$symbol[train_indices],
  date = q_metadata$date[train_indices],
  y = model_signal$actuals$train,
  yhat = model_signal$predictions$train,
  close = q_targets$close_log[train_indices]
)

df_val <- tibble(
  symbol = q_metadata$symbol[val_indices],
  date = q_metadata$date[val_indices],
  y = model_signal$actuals$val,
  yhat = model_signal$predictions$val,
  close = q_targets$close_log[val_indices]
)

df_test <- tibble(
  symbol = q_metadata$symbol[test_indices],
  date = q_metadata$date[test_indices],
  y = model_signal$actuals$test,
  yhat = model_signal$predictions$test,
  close = q_targets$close_log[test_indices]
)

#
# PLOTS.
#
if (FALSE) {
  plot_metrics_comparison(model_signal)
  plot_feature_importance(model_signal, top_n = 20)
  plot_all_predictions(model_signal)
  plot_predictions_vs_actuals(model_signal, "test")
  plot_residuals(model_signal, "test")
  plot_residual_distribution(model_signal, "test")
  plot_predictions_vs_actuals(model_signal, "val")
  plot_residuals(model_signal, "val")
  plot_predictions_vs_actuals(model_signal, "train")
  plot_residuals(model_signal, "train")
  if (FALSE) {
    plot_xgboost_trees(model_signal, tree_indices = c(0, 1, 2, 3, 4))
  }
}
