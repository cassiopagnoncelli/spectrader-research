if (FALSE) {
  devtools::load_all()

  source("rd/models/excursion/1_etl.R")
  source("rd/models/excursion/2_feature_engineering.R")
  source("rd/models/excursion/3_splits.R")
}

source("rd/models/excursion/exploratory_analysis/entry_model.R")

# FEATURE ENRICHMENT.
# Training further predictors.
#
# Output: yhat H and its normalised nH datasets.
#
start_time <- Sys.time()

# Define quantile specifications for excursion targets.
quantile_specs <- list(
  excursion_high = list(
    y_col = "excursion_high",
    taus = c(q980 = 0.98, q993 = 0.993, q999 = 0.999)
  ),
  excursion_low = list(
    y_col = "excursion_low",
    taus = c(q980 = 0.98, q993 = 0.993, q999 = 0.999)
  )
)

# Define regular model specifications: model_name -> Y column mapping.
regular_model_specs <- list(
  # kurtosis = "kurtosis",
  # skewness = "skewness"
)

# Train qboost models for excursion targets.
qboost_models <- list()
for (target_name in names(quantile_specs)) {
  spec <- quantile_specs[[target_name]]
  qboost_models[[target_name]] <- list()

  for (tau_name in names(spec$taus)) {
    model_key <- paste0(target_name, "_", tau_name)
    var_name <- paste0("train_", model_key)

    if (exists(var_name)) {
      message(sprintf("  fwd forecasts model `%s` already trained, skipping training step", model_key))
      qboost_models[[target_name]][[tau_name]] <- get(var_name)
    } else {
      message(sprintf("  training fwd goals: %s (tau = %.3f)", model_key, spec$taus[tau_name]))

      model <- qboost::qboost(
        x = zX[train_idx, ],
        y = Y[[spec$y_col]][train_idx],
        tau = spec$taus[tau_name],
        nrounds = 2000,
        nfolds = 3,
        params = list(
          num_leaves = 96,
          max_depth = 8,
          learning_rate = 0.07,
          cat_smooth = 10
        ),
        early_stopping_rounds = 80,
        seed = 1
      )

      # Generate predictions for all splits
      model$predictions <- list(
        train = predict(model, zX[train_idx, ]),
        val = predict(model, zX[val_idx, ]),
        test = predict(model, zX[test_idx, ])
      )

      assign(var_name, model, envir = .GlobalEnv)
      qboost_models[[target_name]][[tau_name]] <- model
    }
  }
}

# Train regular stacked models (only if any are specified).
trained_models <- list()
if (length(regular_model_specs) > 0) {
  trained_models <- lapply(names(regular_model_specs), function(model_name) {
    var_name <- paste0("train_", model_name)

    if (exists(var_name)) {
      message(sprintf("  fwd forecast qboost `%s` already trained, skipping training step", model_name))
      return(get(var_name))
    }

    model <- train_stacked_model(
      train_idx, val_idx, test_idx,
      X = zX,
      y = Y[[regular_model_specs[[model_name]]]],
      aux = list(),
      cache = NULL,
      verbose = FALSE
    )

    assign(var_name, model, envir = .GlobalEnv)
    message(sprintf("  %s", model_name))
    model
  })
  names(trained_models) <- names(regular_model_specs)
} else {
  message("  no fwd yhat regular (non-tail) models specified, skipping training")
}

message(sprintf(
  "  training complete in %0.2f mins",
  as.numeric(Sys.time() - start_time, units = "mins")
))

# Build predictions tibble dynamically.
# Create columns for qboost models
qboost_cols <- list()
for (target_name in names(quantile_specs)) {
  for (tau_name in names(quantile_specs[[target_name]]$taus)) {
    col_name <- paste0(target_name, "_", tau_name, "_hat")
    qboost_cols[[col_name]] <- rep(NA, nrow(zX))
  }
}

# Create columns for regular models (only if any are specified)
if (length(regular_model_specs) > 0) {
  regular_cols <- setNames(
    lapply(names(regular_model_specs), function(name) rep(NA, nrow(zX))),
    paste0(names(regular_model_specs), "_hat")
  )
  # Combine all columns
  H <- tibble::as_tibble(c(qboost_cols, regular_cols))
} else {
  # Only qboost columns
  H <- tibble::as_tibble(qboost_cols)
}

# Fill in qboost predictions
for (target_name in names(quantile_specs)) {
  for (tau_name in names(quantile_specs[[target_name]]$taus)) {
    col_name <- paste0(target_name, "_", tau_name, "_hat")
    model <- qboost_models[[target_name]][[tau_name]]

    H[train_idx, col_name] <- model$predictions$train
    H[val_idx, col_name] <- model$predictions$val
    H[test_idx, col_name] <- model$predictions$test
  }
}

# Fill in regular model predictions (only if any models were trained)
if (length(trained_models) > 0) {
  for (split in c("train", "val", "test")) {
    idx <- get(paste0(split, "_idx"))
    for (model_name in names(trained_models)) {
      col_name <- paste0(model_name, "_hat")
      H[idx, col_name] <- trained_models[[model_name]]$predictions[[split]]
    }
  }
}

# Scale H to nH based on train set statistics.
# Only perform scaling if there are columns to scale
if (ncol(H) > 0) {
  H_scaled <- scale(H[train_idx, ])
  H_center <- attr(H_scaled, "scaled:center")
  H_scales <- attr(H_scaled, "scaled:scale")

  nH <- tibble::as_tibble(
    setNames(
      lapply(names(H), function(col) rep(NA, nrow(zX))),
      names(H)
    )
  )

  nH[stages_idx, ] <- scale_new_data(
    H[stages_idx, ],
    center = H_center,
    scale = H_scales
  )

  rm(H_scaled)
  gc()
} else {
  # If no columns to scale, create an empty tibble with correct number of rows
  nH <- tibble::tibble(.rows = nrow(zX))
  message("  no features to enrich - H has 0 columns")
}

message("Feature enrichment complete")
