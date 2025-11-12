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
fets::fwd(q, inplace = TRUE)
fets::add_vix(q, vix)
q <- fets::fe(q, inplace = FALSE) %>% na.omit

#
# PREPROCESSING.
#
q_metadata <- q[, .(symbol, date)]
q_targets <- q[, .SD, .SDcols = fets::fwd_methods()]
q_X <- q[, .SD, .SDcols = !c("symbol", "date", "close", fets::fwd_methods())]

# Set splits.
train_indices <- which(q$date <= as.Date("2024-01-30"))
val_indices <- which(q$date > as.Date("2024-01-30") & q$date <= as.Date("2024-12-31"))
test_indices <- which(q$date >= as.Date("2025-01-01"))

train_data <- q_X[train_indices, ]
val_data <- q_X[val_indices, ]
test_data <- q_X[test_indices, ]

#
# Training
#
model_name <- "stock_crossover__entry__extreme_low_identity"
ckm <- cache_key(existing_key = "-500rounds-", ext = "rds", fun = model_name)
model_signal <- train_stacked_model(
  train_indices = train_indices,
  val_indices = val_indices,
  test_indices = test_indices,
  X = q_X,
  y = q_targets$extreme_high_log,
  aux = list(
    y1 = q_targets$extreme_low_log,
    y2 = q_targets$mass_high_identity,
    y3 = q_targets$mass_low_identity,
    y4 = q_targets$sharpe_high,
    y5 = q_targets$sharpe_low,
    y6 = q_targets$close_log,
    y7 = q_targets$mean_log,
    y8 = q_targets$de_log,
    y9 = q_targets$dm_log
  ),
  cache = ckm,
  verbose = TRUE
)

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

