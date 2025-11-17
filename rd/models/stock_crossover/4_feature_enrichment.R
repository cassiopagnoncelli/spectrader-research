if (FALSE) {
  # devtools::load_all()

  source("rd/models/stock_crossover/1_etl.R")
  source("rd/models/stock_crossover/2_feature_engineering.R")
  source("rd/models/stock_crossover/3_splits.R")
}

source("rd/models/stock_crossover/exploratory_analysis/entry_model.R")

# FEATURE ENRICHMENT.
# Training further predictors.
#
# Output: yhat H and its normalised nH datasets.
#
start_time <- Sys.time()

if (exists("train_ehi")) {
  message("Entry models `ehi`` already trained. Skipping training step.")
} else {
  train_ehi <- train_stacked_model(
    train_idx,
    val_idx,
    test_idx,
    X = zX,
    y = Y$extreme_high_identity,
    aux = list(),
    cache = NULL,
    verbose = FALSE
  )
  message("  extreme high identity")
}

if (exists("train_eli")) {
  message("Entry models `eli`` already trained. Skipping training step.")
} else {
  train_eli <- train_stacked_model(
    train_idx,
    val_idx,
    test_idx,
    X = zX,
    y = Y$extreme_low_identity,
    aux = list(),
    cache = NULL,
    verbose = FALSE
  )
  message("  extreme low identity")
}

if (exists("train_pas")) {
  message("Entry models `pas`` already trained. Skipping training step.")
} else {
  train_pas <- train_stacked_model(
    train_idx,
    val_idx,
    test_idx,
    X = zX,
    y = Y$pas,
    aux = list(),
    cache = NULL,
    verbose = FALSE
  )
  message("  pas")
}

if (exists("train_kurtosis")) {
  message("Entry models `kurtosis`` already trained. Skipping training step.")
} else {
  train_kurtosis <- train_stacked_model(
    train_idx,
    val_idx,
    test_idx,
    X = zX,
    y = Y$kurtosis,
    aux = list(),
    cache = NULL,
    verbose = FALSE
  )
  message("  kurtosis")
}

if (exists("train_mh")) {
  message("Entry models `mh`` already trained. Skipping training step.")
} else {
  train_mh <- train_stacked_model(
    train_idx,
    val_idx,
    test_idx,
    X = zX,
    y = Y$mass_high,
    aux = list(),
    cache = NULL,
    verbose = FALSE
  )
  message("  mass high")
}

if (exists("train_ml")) {
  message("Entry models `ml`` already trained. Skipping training step.")
} else {
  train_ml <- train_stacked_model(
    train_idx,
    val_idx,
    test_idx,
    X = zX,
    y = Y$mass_low,
    aux = list(),
    cache = NULL,
    verbose = FALSE
  )
  message("  mass low")
}

message(sprintf("Feature enrichment complete in %0.2f mins", 
                as.numeric(Sys.time() - start_time, units = "mins")))

# Build predictions tibble.
H <- tibble::tibble(
  extreme_high_identity_hat = rep(NA, nrow(zX)),
  extreme_low_identity_hat = rep(NA, nrow(zX)),
  pas_hat = rep(NA, nrow(zX)),
  kurtosis_hat = rep(NA, nrow(zX)),
  mass_high_hat = rep(NA, nrow(zX)),
  mass_low_hat = rep(NA, nrow(zX))
)

H[train_idx, ] <- data.frame(
  extreme_high_identity_hat = train_ehi$predictions$train,
  extreme_low_identity_hat = train_eli$predictions$train,
  pas_hat = train_pas$predictions$train,
  kurtosis_hat = train_kurtosis$predictions$train,
  mass_high_hat = train_mh$predictions$train,
  mass_low_hat = train_ml$predictions$train
)

H[val_idx, ] <- data.frame(
  extreme_high_identity_hat = train_ehi$predictions$val,
  extreme_low_identity_hat = train_eli$predictions$val,
  pas_hat = train_pas$predictions$val,
  kurtosis_hat = train_kurtosis$predictions$val,
  mass_high_hat = train_mh$predictions$val,
  mass_low_hat = train_ml$predictions$val
)

H[test_idx, ] <- data.frame(
  extreme_high_identity_hat = train_ehi$predictions$test,
  extreme_low_identity_hat = train_eli$predictions$test,
  pas_hat = train_pas$predictions$test,
  kurtosis_hat = train_kurtosis$predictions$test,
  mass_high_hat = train_mh$predictions$test,
  mass_low_hat = train_ml$predictions$test
)

# Scale H to nH based on train set statistics.
H_scaled <- scale(H[train_idx, ])
H_center <- attr(H_scaled, "scaled:center")
H_scales <- attr(H_scaled, "scaled:scale")

nH <- tibble::tibble(
  extreme_high_identity_hat = rep(NA, nrow(zX)),
  extreme_low_identity_hat = rep(NA, nrow(zX)),
  pas_hat = rep(NA, nrow(zX)),
  kurtosis_hat = rep(NA, nrow(zX)),
  mass_high_hat = rep(NA, nrow(zX)),
  mass_low_hat = rep(NA, nrow(zX))
)

nH[c(train_idx, val_idx, test_idx), ] <- scale_new_data(
  H[c(train_idx, val_idx, test_idx), ],
  center = H_center,
  scale = H_scales
)

rm(H_scaled)
gc()
