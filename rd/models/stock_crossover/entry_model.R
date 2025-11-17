devtools::load_all()

source("rd/models/stock_crossover/etl.R")

#
# Training
#
fets::fwd_methods()

fit_ehi <- qboost::qboost(
  x = nX[train_idx, ],
  y = Y$extreme_high_identity[train_idx],
  tau = 0.993,
  nrounds = 500,
  nfolds = 5,
  params = list(),
  early_stopping_rounds = 50,
  seed = 1,
)

fit_eli <- qboost::qboost(
  x = nX[train_idx, ],
  y = Y$extreme_low_identity[train_idx],
  tau = 0.996,
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
  Y[, c("extreme_high_identity", "extreme_low_identity")],
  P,
  nX
)

lock_all(P, mnXYP)
