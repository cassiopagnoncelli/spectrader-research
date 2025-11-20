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

# Define model specifications: model_name -> Y column mapping.
model_specs <- list(
  excursion_high = "excursion_high",
  excursion_low = "excursion_low",
  qeh = "excursion_high",
  qel = "excursion_low",
  kurtosis = "kurtosis"
)

# Train models with existence check.
trained_models <- lapply(names(model_specs), function(model_name) {
  var_name <- paste0("train_", model_name)
  
  if (exists(var_name)) {
    message(sprintf("Entry models `%s` already trained. Skipping training step.", model_name))
    return(get(var_name))
  }
  
  model <- train_stacked_model(
    train_idx, val_idx, test_idx,
    X = zX,
    y = Y[[model_specs[[model_name]]]],
    aux = list(),
    cache = NULL,
    verbose = FALSE
  )
  
  assign(var_name, model, envir = .GlobalEnv)
  message(sprintf("  %s", model_name))
  model
})
names(trained_models) <- names(model_specs)

message(sprintf("Feature enrichment complete in %0.2f mins", 
                as.numeric(Sys.time() - start_time, units = "mins")))

# Build predictions tibble dynamically.
H <- tibble::as_tibble(
  setNames(
    lapply(names(model_specs), function(name) rep(NA, nrow(zX))),
    paste0(names(model_specs), "_hat")
  )
)

for (split in c("train", "val", "test")) {
  idx <- get(paste0(split, "_idx"))
  H[idx, ] <- as.data.frame(lapply(trained_models, function(m) m$predictions[[split]]))
}

# Scale H to nH based on train set statistics.
H_scaled <- scale(H[train_idx, ])
H_center <- attr(H_scaled, "scaled:center")
H_scales <- attr(H_scaled, "scaled:scale")

nH <- tibble::as_tibble(
  setNames(
    lapply(names(H), function(col) rep(NA, nrow(zX))),
    names(H)
  )
)

nH[c(train_idx, val_idx, test_idx), ] <- scale_new_data(
  H[c(train_idx, val_idx, test_idx), ],
  center = H_center,
  scale = H_scales
)

rm(H_scaled)
gc()
