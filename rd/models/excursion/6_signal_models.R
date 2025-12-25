if (FALSE) {
  devtools::load_all()

  source("rd/models/excursion/1_etl.R")
  source("rd/models/excursion/2_feature_engineering.R")
  source("rd/models/excursion/3_splits.R")
  source("rd/models/excursion/4_feature_enrichment.R")
  source("rd/models/excursion/5_datasets.R")
}

# SIGNAL MODELS.
# Train models for diverse fwd goals.
#
# Outpput: fwd predictions P and complessive dataset mnXYP.
#
message("  training signal models")

start_time <- Sys.time()

fets::fwd_goals()

fit_start_time <- Sys.time()
signal_qeh <- qboost::qtail(
  x = nX[train_idx, ],
  y = Y$qeh[train_idx],
  taus = c(0.95, 0.98, 0.99, 0.994, 0.999),
  tail = "upper",
  threshold_tau = 0.98,
  verbose = TRUE
) # ~8 min
message(
  sprintf(
    "  signal qeh (qtail) complete in %0.0f secs",
    as.numeric(Sys.time() - fit_start_time, units = "secs")
  )
)

fit_start_time <- Sys.time()
signal_qel <- qboost::qtail(
  x = nX[train_idx, ],
  y = Y$qel[train_idx],
  taus = c(0.95, 0.98, 0.99, 0.994, 0.999),
  tail = "upper",
  threshold_tau = 0.98,
  verbose = TRUE
) # ~6 min
message(
  sprintf(
    "  signal qel (qtail) complete in %0.0f secs",
    as.numeric(Sys.time() - fit_start_time, units = "secs")
  )
)

message(
  sprintf(
    "Signal models trained in %0.0f secs",
    as.numeric(Sys.time() - start_time, units = "secs")
  )
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
  yhat_qeh = predict(signal_qeh, nX[stages_idx, ]),
  yhat_qel = predict(signal_qel, nX[stages_idx, ])
)

mnXYP <- tibble::tibble(
  meta,
  Y[, c("qeh", "qel")],
  P,
  nX
)

lock_all(P, mnXYP)
