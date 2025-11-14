# set.seed(123)

# Stacked XGBoost Model for Stock Crossover Prediction
# This file contains the model training logic with caching support
train_stacked_model <- function(train_indices, val_indices, test_indices,
                                X, y, aux = list(),
                                cache = NULL,
                                verbose = TRUE) {
  if (!is.null(cache)) {
    results <- load_cache(cache)
    if (!is.null(results)) {
      print_stacked_model_metrics(results)
      return(results)
    }
    cat("Cache miss, proceeding to train a new model...\n")
  }
  results <- perform_train_stacked_model(
    train_indices, val_indices, test_indices,
    X, y, aux,
    verbose = verbose
  )
  if (!is.null(cache)) {
    save_cache(cache, results)
  }
  results
}

sm_level_1 <- list(
  hyperparams = list(
    objective = "reg:squarederror",
    eta = 0.2,              # conservative: <.1, aggressive: >.1
    max_depth = 3,           # default: 5
    min_child_weight = 3,    # default: 3
    subsample = 1,           # default: .8
    colsample_bytree = 1     # default: .8
  ),
  control = list(
    nrounds = 200,
    early_stopping_rounds = 20
  )
)

sm_level_2 <- list(
  hyperparams = list(
    objective = "reg:squarederror",
    eta = 0.2,              # conservative: <.1, aggressive: >.1
    max_depth = 3,           # default: 5
    min_child_weight = 2,    # default: 3
    subsample = 0.8,
    colsample_bytree = 0.8
  ),
  control = list(
    nrounds = 250,
    early_stopping_rounds = 20
  )
)

perform_train_stacked_model <- function(
  train_indices, val_indices, test_indices, X, y, aux, verbose = TRUE
) {
  # LEVEL 1: Train XGBoost models for each auxiliary target
  n_aux <- length(aux)

  # Check if aux is empty - if so, skip Level 1 entirely
  if (n_aux == 0) {
    if (verbose) {
      cat("\n=== No auxiliary models specified, training final model directly ===\n")
    }
    # Use original features directly
    X_stacked <- X
    models_level1 <- list()
  } else {
    if (verbose) {
      cat(sprintf("\n=== LEVEL 1: Training %d First-Level Models ===\n", n_aux))
    }

    # Train all first-level models dynamically based on aux list
    models_level1 <- list()
    for (i in seq_along(aux)) {
      model_name <- sprintf("Model %s", names(aux)[i])
      models_level1[[i]] <- train_level1_model(
        X,
        aux[[i]],
        train_indices,
        val_indices,
        model_name,
        verbose = verbose
      )
    }

    # Generate predictions from first-level models on all splits
    if (verbose) {
      cat("\n=== Generating Level 1 Predictions ===\n")
    }
    X_matrix <- as.matrix(X)
    dmatrix_all <- xgboost::xgb.DMatrix(data = X_matrix)

    # Generate predictions dynamically for all auxiliary models
    level1_preds <- list()
    for (i in seq_along(models_level1)) {
      level1_preds[[i]] <- predict(models_level1[[i]], dmatrix_all)
    }

    # LEVEL 2: Create stacked dataset with X + predictions
    if (verbose) {
      cat(sprintf("✓ Level 1 predictions generated for all %d models\n", n_aux))
      cat("\n=== LEVEL 2: Preparing Stacked Dataset ===\n")
    }

    # Combine original features with level 1 predictions
    pred_matrix <- do.call(cbind, level1_preds)
    colnames(pred_matrix) <- paste0(names(aux), "_pred")
    X_stacked <- cbind(X, pred_matrix)

    if (verbose) {
      cat(sprintf("Stacked features: %d (original) + %d (predictions) = %d total\n",
                  ncol(X), n_aux, ncol(X_stacked)))
    }
  }

  if (verbose) {
    cat("\n=== Training Final Stacked Model ===\n")
  }

  # Train final XGBoost model
  X_train_stacked <- as.matrix(X_stacked[train_indices, ])
  y_train <- y[train_indices]
  X_val_stacked <- as.matrix(X_stacked[val_indices, ])
  y_val <- y[val_indices]

  dtrain_final <- xgboost::xgb.DMatrix(data = X_train_stacked, label = y_train)
  dval_final <- xgboost::xgb.DMatrix(data = X_val_stacked, label = y_val)

  watchlist_final <- list(train = dtrain_final, val = dval_final)
  model_final <- xgboost::xgb.train(
    params = sm_level_2$hyperparams,
    data = dtrain_final,
    nrounds = sm_level_2$control$nrounds,
    watchlist = watchlist_final,
    early_stopping_rounds = sm_level_2$control$early_stopping_rounds,
    verbose = ifelse(verbose, 1, 0)
  )

  if (verbose) {
    cat(sprintf("\nBest iteration: %d\n", model_final$best_iteration))
  }

  # EVALUATION
  # Predictions on all splits
  X_train_stacked_dm <- xgboost::xgb.DMatrix(data = X_train_stacked)
  X_val_stacked_dm <- xgboost::xgb.DMatrix(data = X_val_stacked)
  X_test_stacked <- as.matrix(X_stacked[test_indices, ])
  X_test_stacked_dm <- xgboost::xgb.DMatrix(data = X_test_stacked)

  pred_train <- predict(model_final, X_train_stacked_dm)
  pred_val <- predict(model_final, X_val_stacked_dm)
  pred_test <- predict(model_final, X_test_stacked_dm)

  y_test <- y[test_indices]

  # Calculate metrics (excluding NAs)
  rmse_train <- sqrt(mean((pred_train - y_train)^2, na.rm = TRUE))
  rmse_val <- sqrt(mean((pred_val - y_val)^2, na.rm = TRUE))
  rmse_test <- sqrt(mean((pred_test - y_test)^2, na.rm = TRUE))

  mae_train <- mean(abs(pred_train - y_train), na.rm = TRUE)
  mae_val <- mean(abs(pred_val - y_val), na.rm = TRUE)
  mae_test <- mean(abs(pred_test - y_test), na.rm = TRUE)

  r2_train <- 1 - sum((pred_train - y_train)^2, na.rm = TRUE) / sum((y_train - mean(y_train, na.rm = TRUE))^2, na.rm = TRUE)
  r2_val <- 1 - sum((pred_val - y_val)^2, na.rm = TRUE) / sum((y_val - mean(y_val, na.rm = TRUE))^2, na.rm = TRUE)
  r2_test <- 1 - sum((pred_test - y_test)^2, na.rm = TRUE) / sum((y_test - mean(y_test, na.rm = TRUE))^2, na.rm = TRUE)

  # Feature importance
  importance_matrix <- xgboost::xgb.importance(model = model_final)

  # Prepare results
  results <- list(
    models_level1 = models_level1,
    model_final = model_final,
    predictions = list(
      train = pred_train,
      val = pred_val,
      test = pred_test
    ),
    actuals = list(
      train = y_train,
      val = y_val,
      test = y_test
    ),
    metrics = list(
      train = c(rmse = rmse_train, mae = mae_train, r2 = r2_train),
      val = c(rmse = rmse_val, mae = mae_val, r2 = r2_val),
      test = c(rmse = rmse_test, mae = mae_test, r2 = r2_test)
    ),
    importance = importance_matrix
  )


  print_stacked_model_metrics(results)

  results
}

print_stacked_model_metrics <- function(results) {
  cat(sprintf(
    paste0(
      "Stacked Model: performance metrics\n",
      "Train - RMSE: %.6f, MAE: %.6f, R²: %.4f\n",
      "Val   - RMSE: %.6f, MAE: %.6f, R²: %.4f\n",
      "Test  - RMSE: %.6f, MAE: %.6f, R²: %.4f\n"
    ),
    results$metrics$train["rmse"],
    results$metrics$train["mae"],
    results$metrics$train["r2"],
    results$metrics$val["rmse"],
    results$metrics$val["mae"],
    results$metrics$val["r2"],
    results$metrics$test["rmse"],
    results$metrics$test["mae"],
    results$metrics$test["r2"]
  ))
}

# Function to train a single first-level model
train_level1_model <- function(X_data, y_aux, train_idx, val_idx, model_name, verbose = TRUE) {
  if (verbose) {
    cat(sprintf("\nTraining %s...\n", model_name))
  }

  # Prepare data
  X_train <- as.matrix(X_data[train_idx, ])
  y_train <- y_aux[train_idx]
  X_val <- as.matrix(X_data[val_idx, ])
  y_val <- y_aux[val_idx]

  # Create DMatrix
  dtrain <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
  dval <- xgboost::xgb.DMatrix(data = X_val, label = y_val)

  # Train with early stopping
  watchlist <- list(train = dtrain, val = dval)
  model <- xgboost::xgb.train(
    params = sm_level_1$hyperparams,
    data = dtrain,
    nrounds = sm_level_1$control$nrounds,
    watchlist = watchlist,
    early_stopping_rounds = sm_level_1$control$early_stopping_rounds,
    verbose = ifelse(verbose, 1, 0)
  )

  if (verbose) {
    cat(sprintf(
      "  Best iteration: %d\n  Train RMSE: %.6f\n  Val RMSE: %.6f\n",
      model$best_iteration,
      model$evaluation_log$train_rmse_mean[model$best_iteration],
      model$evaluation_log$val_rmse_mean[model$best_iteration]
    ))
  }

  model
}

build_df_stages <- function(model, stage) {
  stage_indices <- if (stage == "train") {
    train_indices
  } else if (stage == "val") {
    val_indices
  } else if (stage == "test") {
    test_indices
  } else {
    stop("Invalid stage")
  }

  tibble(
    symbol = meta$symbol[stage_indices],
    date = meta$date[stage_indices],
    y = model$actuals[[stage]],
    yhat = model$predictions[[stage]],
    close = ys$close_identity[stage_indices]
  )
}
