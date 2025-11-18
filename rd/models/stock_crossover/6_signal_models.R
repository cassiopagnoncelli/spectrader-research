if (FALSE) {
  # devtools::load_all()

  source("rd/models/stock_crossover/1_etl.R")
  source("rd/models/stock_crossover/2_feature_engineering.R")
  source("rd/models/stock_crossover/3_splits.R")
  source("rd/models/stock_crossover/4_feature_enrichment.R")
  source("rd/models/stock_crossover/5_datasets.R")
}

# SIGNAL MODELS.
# Train models for diverse fwd goals.
#
# Outpput: fwd predictions P and complessive dataset mnXYP.
#
fets::fwd_goals()

ehi_tau <- 0.995
eli_tau <- 0.999

fit_ehi <- qboost::qboost(
  x = nX[train_idx, ],
  y = Y$excursion_high[train_idx],
  tau = ehi_tau,
  nrounds = 500,
  nfolds = 5,
  params = list(),
  early_stopping_rounds = 50,
  seed = 1,
)

fit_eli <- qboost::qboost(
  x = nX[train_idx, ],
  y = Y$excursion_low[train_idx],
  tau = eli_tau,
  nrounds = 800,
  nfolds = 8,
  params = list(),
  early_stopping_rounds = 50,
  seed = 1,
)

# Predictions
if (exists("P")) { unlock_all(P) }
if (exists("mnXYP")) { unlock_all(mnXYP) }

P <- tibble::tibble(
  yhat_eli = rep(NA, nrow(nX)),
  yhat_ehi = rep(NA, nrow(nX))
)
P[stages_idx, ] <- tibble::tibble(
  yhat_ehi = predict(fit_ehi, nX[stages_idx, ]),
  yhat_eli = predict(fit_eli, nX[stages_idx, ])
)

mnXYP <- tibble::tibble(
  meta,
  Y[, c("excursion_high", "excursion_low")],
  P,
  nX
)

lock_all(P, mnXYP)
