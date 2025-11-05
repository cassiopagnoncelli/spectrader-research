devtools::load_all()

options(scipen = 999)

source("rd/models/stock_crossover/features.R")
source("rd/models/stock_crossover/regression_model.R")
source("rd/models/stock_crossover/regression_plots.R")

#
# ETL.
#
fetl <- Fetl$new()
features_params <- list(
  companies = 13000,          # Cached: 13000
  days = 15,                  # Cached: 10*, 15, 22*, 44*
  methods = c(
    "extreme_high_identity",  # y
    "extreme_low_identity",   # y_1
    "mass_high_log",          # y_2
    "mass_low_log",           # y_3
    "dm_log",                 # y_4
    "sharpe_high",            # y_5
    "sharpe_low",             # y_6
    "close_identity"          # y_7
  )
)
ckf <- cache_key(params = features_params, ext = "rds", fun = "prepare_fwd")
features <- prepare_fwd(
  fetl,
  companies = features_params$companies,
  days = features_params$days,
  methods = features_params$methods,
  cache = ckf
)
fwd <- features$fwd
fwd_metadata <- features$fwd_metadata
fwd_joint <- tibble(fwd_metadata, fwd)
# fwd_joint %>% glimpse

#
# PREPROCESSING.
# XyN are data frames with target variable y_N and features X, each requiring
# a model to predict y_N. y_N are strong predictors of future movements, like
# extremes, mass movements, and sharpe ratios.
#
X <- fwd %>%
  select(-y, -y_1, -y_2, -y_3, -y_4, -y_5, -y_6, -y_7)

Xy1 <- cbind(y = fwd$y_1, X)
Xy2 <- cbind(y = fwd$y_2, X)
Xy3 <- cbind(y = fwd$y_3, X)
Xy4 <- cbind(y = fwd$y_4, X)
Xy5 <- cbind(y = fwd$y_5, X)
Xy6 <- cbind(y = fwd$y_6, X)

train_indices <- which(fwd_metadata$date <= as.Date('2024-06-30'))
val_indices <- which(fwd_metadata$date > as.Date('2024-06-30') &
                       fwd_metadata$date <= as.Date('2024-12-31'))
test_indices <- which(fwd_metadata$date >= as.Date('2025-01-20'))

train_data <- fwd[train_indices, ]
val_data <- fwd[val_indices, ]
test_data <- fwd[test_indices, ]

#
# TRAINING.
# Stacked model
#
ckm <- cache_key(existing_key = ckf$key, ext = "rds", fun = "stacked_xgboost")
results <- train_stacked_model(
  X = X,
  fwd = fwd,
  train_indices = train_indices,
  val_indices = val_indices,
  test_indices = test_indices,
  Xy1 = Xy1,
  Xy2 = Xy2,
  Xy3 = Xy3,
  Xy4 = Xy4,
  Xy5 = Xy5,
  Xy6 = Xy6,
  cache = ckm
)

# PLOTS
if (FALSE) {
  plot_metrics_comparison(results)
  plot_feature_importance(results, top_n = 20)
  plot_all_predictions(results)
  plot_predictions_vs_actuals(results, "test")
  plot_residuals(results, "test")
  plot_residual_distribution(results, "test")
  plot_predictions_vs_actuals(results, "val")
  plot_residuals(results, "val")
  plot_predictions_vs_actuals(results, "train")
  plot_residuals(results, "train")
  plot_xgboost_trees(results, tree_indices = c(0, 1, 2, 3, 4))
}
