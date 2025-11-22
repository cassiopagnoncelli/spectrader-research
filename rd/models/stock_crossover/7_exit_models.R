if (FALSE) {
  # devtools::load_all()

  source("rd/models/stock_crossover/1_etl.R")
  source("rd/models/stock_crossover/2_feature_engineering.R")
  source("rd/models/stock_crossover/3_splits.R")
  source("rd/models/stock_crossover/4_feature_enrichment.R")
  source("rd/models/stock_crossover/5_datasets.R")
  source("rd/models/stock_crossover/6_signal_models.R")
}

# EXIT MODELS.
# Position exit models.
#
# Output: models fits.
#

# --
# Decaying Quantile Regression (DQR) for exit signals.

# General DQR formula for exit models
dqr_general_formula <- S ~ t + S_1 + S_2 + R_1 + R_2 + vix_2 + wh + vix
dqr_general_formula

# Fit exit dqr on train subset
qeh_q <- mnXYP$yhat_qeh[train_idx] %>% quantile(qeh_tau)
qeh_q

dqr_signals_train <- mnXYP[train_idx, ] %>%
  dplyr::filter(yhat_qeh > qeh_q) %>%
  filter_signals(within_days = 20) %>%
  dplyr::arrange(date)

dqr_signals_train

start_time <- Sys.time()

dqr_fits <- train_dqr(
  dqr_signals_train,
  quotes = mcnXY[train_idx, ],
  taus = c(.92, .86, .82, .75),
  formulas = list(
    dqr_general_formula,
    dqr_general_formula,
    dqr_general_formula,
    dqr_general_formula
  ),
  max_position_days = 20
)

message(
  sprintf(
    "DQR exit models fitted in %0.0f secs",
    as.numeric(Sys.time() - start_time, units = "secs")
  )
)
