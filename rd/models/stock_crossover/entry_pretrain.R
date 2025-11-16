source("rd/models/stock_crossover/exploratory_analysis/entry_model.R")

if (exists("train_ehi")) {
  message("Entry models `ehi`` already trained. Skipping training step.")
} else {
  train_ehi <- train_stacked_model(
    train_idx,
    val_idx,
    test_idx,
    X = nX,
    y = Y$extreme_high_identity,
    aux = list(),
    cache = NULL,
    verbose = TRUE
  )
}

if (exists("train_eli")) {
  message("Entry models `eli`` already trained. Skipping training step.")
} else {
  train_eli <- train_stacked_model(
    train_idx,
    val_idx,
    test_idx,
    X = nX,
    y = Y$extreme_low_identity,
    aux = list(),
    cache = NULL,
    verbose = TRUE
  )
}

if (exists("train_pas")) {
  message("Entry models `pas`` already trained. Skipping training step.")
} else {
  train_pas <- train_stacked_model(
    train_idx,
    val_idx,
    test_idx,
    X = nX,
    y = Y$pas,
    aux = list(),
    cache = NULL,
    verbose = TRUE
  )
}

if (exists("train_kurtosis")) {
  message("Entry models `kurtosis`` already trained. Skipping training step.")
} else {
  train_kurtosis <- train_stacked_model(
    train_idx,
    val_idx,
    test_idx,
    X = nX,
    y = Y$kurtosis,
    aux = list(),
    cache = NULL,
    verbose = TRUE
  )
}

if (exists("train_mh")) {
  message("Entry models `mh`` already trained. Skipping training step.")
} else {
  train_mh <- train_stacked_model(
    train_idx,
    val_idx,
    test_idx,
    X = nX,
    y = Y$mass_high,
    aux = list(),
    cache = NULL,
    verbose = TRUE
  )
}

if (exists("train_ml")) {
  message("Entry models `ml`` already trained. Skipping training step.")
} else {
  train_ml <- train_stacked_model(
    train_idx,
    val_idx,
    test_idx,
    X = nX,
    y = Y$mass_low,
    aux = list(),
    cache = NULL,
    verbose = TRUE
  )
}

H <- tibble::tibble(
  ehi_hat = rep(NA, nrow(nX)),
  eli_hat = rep(NA, nrow(nX)),
  pas_hat = rep(NA, nrow(nX)),
  kurtosis_hat = rep(NA, nrow(nX)),
  mh_hat = rep(NA, nrow(nX)),
  ml_hat = rep(NA, nrow(nX))
)

H[train_idx, ] <- data.frame(
  ehi_hat = train_ehi$predictions$train,
  eli_hat = train_eli$predictions$train,
  pas_hat = train_pas$predictions$train,
  kurtosis_hat = train_kurtosis$predictions$train,
  mh_hat = train_mh$predictions$train,
  ml_hat = train_ml$predictions$train
)

H[val_idx, ] <- data.frame(
  ehi_hat = train_ehi$predictions$val,
  eli_hat = train_eli$predictions$val,
  pas_hat = train_pas$predictions$val,
  kurtosis_hat = train_kurtosis$predictions$val,
  mh_hat = train_mh$predictions$val,
  ml_hat = train_ml$predictions$val
)

H[test_idx, ] <- data.frame(
  ehi_hat = train_ehi$predictions$test,
  eli_hat = train_eli$predictions$test,
  pas_hat = train_pas$predictions$test,
  kurtosis_hat = train_kurtosis$predictions$test,
  mh_hat = train_mh$predictions$test,
  ml_hat = train_ml$predictions$test
)

H_scaled <- scale(H[train_idx, ])
H_center <- attr(H_scaled, "scaled:center")
H_scales <- attr(H_scaled, "scaled:scale")

nH <- tibble::tibble(
  ehi_hat = rep(NA, nrow(nX)),
  eli_hat = rep(NA, nrow(nX)),
  pas_hat = rep(NA, nrow(nX)),
  kurtosis_hat = rep(NA, nrow(nX)),
  mh_hat = rep(NA, nrow(nX)),
  ml_hat = rep(NA, nrow(nX))
)

nH[c(train_idx, val_idx, test_idx), ] <- scale_new_data(
  H[c(train_idx, val_idx, test_idx), ],
  center = H_center,
  scale = H_scales
)

rm(H_scaled)
gc()
