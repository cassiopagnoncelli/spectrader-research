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
    y = Y$excursion_high,
    aux = list(),
    cache = NULL,
    verbose = FALSE
  )
  message("  excursion high")
}

if (exists("train_eli")) {
  message("Entry models `eli`` already trained. Skipping training step.")
} else {
  train_eli <- train_stacked_model(
    train_idx,
    val_idx,
    test_idx,
    X = zX,
    y = Y$excursion_low,
    aux = list(),
    cache = NULL,
    verbose = FALSE
  )
  message("  excursion low")
}

if (exists("train_qeh")) {
  message("Entry models `qeh`` already trained. Skipping training step.")
} else {
  train_qeh <- train_stacked_model(
    train_idx,
    val_idx,
    test_idx,
    X = zX,
    y = Y$excursion_high,
    aux = list(),
    cache = NULL,
    verbose = FALSE
  )
  message("  qeh")
}

if (exists("train_qel")) {
  message("Entry models `qel`` already trained. Skipping training step.")
} else {
  train_qel <- train_stacked_model(
    train_idx,
    val_idx,
    test_idx,
    X = zX,
    y = Y$excursion_low,
    aux = list(),
    cache = NULL,
    verbose = FALSE
  )
  message("  qel")
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

message(sprintf("Feature enrichment complete in %0.2f mins", 
                as.numeric(Sys.time() - start_time, units = "mins")))

# Build predictions tibble.
H <- tibble::tibble(
  excursion_high_hat = rep(NA, nrow(zX)),
  excursion_low_hat = rep(NA, nrow(zX)),
  qeh_hat = rep(NA, nrow(zX)),
  qel_hat = rep(NA, nrow(zX)),
  pas_hat = rep(NA, nrow(zX)),
  kurtosis_hat = rep(NA, nrow(zX))
)

H[train_idx, ] <- data.frame(
  excursion_high_hat = train_ehi$predictions$train,
  excursion_low_hat = train_eli$predictions$train,
  qeh_hat = train_qeh$predictions$train,
  qel_hat = train_qel$predictions$train,
  pas_hat = train_pas$predictions$train,
  kurtosis_hat = train_kurtosis$predictions$train
)

H[val_idx, ] <- data.frame(
  excursion_high_hat = train_ehi$predictions$val,
  excursion_low_hat = train_eli$predictions$val,
  qeh_hat = train_qeh$predictions$val,
  qel_hat = train_qel$predictions$val,
  pas_hat = train_pas$predictions$val,
  kurtosis_hat = train_kurtosis$predictions$val
)

H[test_idx, ] <- data.frame(
  excursion_high_hat = train_ehi$predictions$test,
  excursion_low_hat = train_eli$predictions$test,
  qeh_hat = train_qeh$predictions$test,
  qel_hat = train_qel$predictions$test,
  pas_hat = train_pas$predictions$test,
  kurtosis_hat = train_kurtosis$predictions$test
)

# Scale H to nH based on train set statistics.
H_scaled <- scale(H[train_idx, ])
H_center <- attr(H_scaled, "scaled:center")
H_scales <- attr(H_scaled, "scaled:scale")

nH <- tibble::tibble(
  excursion_high_hat = rep(NA, nrow(zX)),
  excursion_low_hat = rep(NA, nrow(zX)),
  qeh_hat = rep(NA, nrow(zX)),
  qel_hat = rep(NA, nrow(zX)),
  pas_hat = rep(NA, nrow(zX)),
  kurtosis_hat = rep(NA, nrow(zX))
)

nH[c(train_idx, val_idx, test_idx), ] <- scale_new_data(
  H[c(train_idx, val_idx, test_idx), ],
  center = H_center,
  scale = H_scales
)

rm(H_scaled)
gc()
