devtools::load_all()

source("rd/models/stock_crossover/etl.R")

#
# Training
#
fets::fwd_methods()

fit_ehi <- lgb_quantile_cv(
  x = as.matrix(nX[train_idx, ]),
  y = Y$extreme_high_identity[train_idx],
  tau = 0.993,
  nfold = 5,
  nrounds = 400
)

fit_eli <- lgb_quantile_cv(
  x = as.matrix(nX[train_idx, ]),
  y = Y$extreme_low_identity[train_idx],
  tau = 0.996,
  nfold = 8,
  nrounds = 600
)

# Predictions
if (exists("P")) { unlock_all(P) }
if (exists("mnXYP")) { unlock_all(mnXYP) }

P <- tibble::tibble(
  yhat_eli = rep(NA, nrow(nX)),
  yhat_ehi = rep(NA, nrow(nX))
)
P[stages_idx, ] <- tibble::tibble(
  yhat_ehi = fit_ehi$predict(as.matrix(nX[stages_idx, ])),
  yhat_eli = fit_eli$predict(as.matrix(nX[stages_idx, ]))
)

mnXYP <- tibble::tibble(
  meta,
  Y[, c("extreme_high_identity", "extreme_low_identity")],
  P,
  nX
)

lock_all(P, mnXYP)
