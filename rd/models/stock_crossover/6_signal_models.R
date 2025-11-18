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

qeh_tau <- 0.6
qel_tau <- 0.995

fit_qeh <- qboost::qboost(
  x = nX[train_idx, ],
  y = Y$qeh[train_idx],
  tau = qeh_tau,
  nrounds = 600,
  nfolds = 6,
  params = list(),
  early_stopping_rounds = 50,
  seed = 1,
)

fit_qel <- qboost::qboost(
  x = nX[train_idx, ],
  y = Y$qel[train_idx],
  tau = qel_tau,
  nrounds = 600,
  nfolds = 6,
  params = list(),
  early_stopping_rounds = 50,
  seed = 1,
)

# Predictions
if (exists("P")) {
  unlock_all(P)
}
if (exists("mnXYP")) {
  unlock_all(mnXYP)
}

P <- tibble::tibble(
  yhat_qeh = rep(NA, nrow(nX)),
  yhat_qel = rep(NA, nrow(nX))
)
P[stages_idx, ] <- tibble::tibble(
  yhat_qeh = predict(fit_qeh, nX[stages_idx, ]),
  yhat_qel = predict(fit_qel, nX[stages_idx, ])
)

mnXYP <- tibble::tibble(
  meta,
  Y[, c("qeh", "qel")],
  P,
  nX
)

lock_all(P, mnXYP)
