devtools::load_all()

source("rd/models/stock_crossover/etl.R")

#
# Training
#
fets::fwd_methods()

# Model for extreme value of extreme high identity
fit_lasso_high <- rqPen::rq.pen(
  x = nX[train_idx, ],
  y = Y$extreme_high_identity[train_idx],
  tau = .993,
  penalty = "LASSO"
)

# Model filter for extreme low identity
tau_low <- quantile(Y$extreme_low_identity[train_idx], probs = 0.9, na.rm = TRUE)
fit_lasso_low <- rqPen::rq.pen(
  x = nX[train_idx, ],
  y = Y$extreme_low_identity[train_idx],
  tau = tau_low,
  penalty = "LASSO"
)

# Predictions
if (exists("P")) {
  unlock_all(P)
}
if (exists("mnXYP")) {
  unlock_all(mnXYP)
}

P <- tibble::tibble(
  yhat_eli = rep(NA, nrow(nX)),
  yhat_ehi = rep(NA, nrow(nX))
)
P[stages_idx, ] <- tibble::tibble(
  yhat_ehi = predict(fit_lasso_high, newx = as.matrix(nX[stages_idx, ]))[, 12],
  yhat_eli = predict(fit_lasso_low, newx = as.matrix(nX[stages_idx, ]))[, 12]
)

mnXYP <- tibble::tibble(
  meta,
  Y[, c("extreme_high_identity", "extreme_low_identity")],
  P,
  nX
)

lock_all(P, mnXYP)
