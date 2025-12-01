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

qeh_tau <- 0.9
qel_tau <- 0.99

fit_start_time <- Sys.time()
fit_qeh <- qboost::qboost(
  x = nX[train_idx, ],
  y = Y$qeh[train_idx],
  tau = qeh_tau,
  nrounds = 1500,
  nfolds = 3,
  params = list(
    num_leaves = 128,
    max_depth = 8,
    learning_rate = 0.07,
    cat_smooth = 10
  ),
  early_stopping_rounds = 65,
  seed = 1
)
message(
  sprintf(
    "  signal qeh (qboost tau = %0.2f) complete in %0.0f secs",
    qeh_tau, as.numeric(Sys.time() - fit_start_time, units = "secs")
  )
)

fit_start_time <- Sys.time()
fit_qel <- qboost::qboost(
  x = nX[train_idx, ],
  y = Y$qel[train_idx],
  tau = qel_tau,
  nrounds = 5000,
  nfolds = 3,
  params = list(
    num_leaves = 128,
    max_depth = 14,
    learning_rate = 0.07,
    cat_smooth = 10
  ),
  early_stopping_rounds = 200,
  seed = 1
)
message(
  sprintf(
    "  signal qel (qboost tau = %0.2f) complete in %0.0f secs",
    qel_tau, as.numeric(Sys.time() - fit_start_time, units = "secs")
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
