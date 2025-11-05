# set.seed(123)

# Stacked XGBoost Model for Stock Crossover Prediction
# This file contains the model training logic with caching support
train_stacked_model <- function(X, fwd, train_indices, val_indices, test_indices,
                                Xy1, Xy2, Xy3, Xy4, Xy5, Xy6,
                                cache = NULL) {

  # Check cache
  if (!is.null(cache)) {
    results <- load_cache(cache)
    cat("✓ Cached model loaded successfully\n")

    # Print cached metrics
    cat("\n=== Cached Performance Metrics ===\n")
    cat(sprintf("Train - RMSE: %.6f, MAE: %.6f, R²: %.4f\n",
                results$metrics$train["rmse"],
                results$metrics$train["mae"],
                results$metrics$train["r2"]))
    cat(sprintf("Val   - RMSE: %.6f, MAE: %.6f, R²: %.4f\n",
                results$metrics$val["rmse"],
                results$metrics$val["mae"],
                results$metrics$val["r2"]))
    cat(sprintf("Test  - RMSE: %.6f, MAE: %.6f, R²: %.4f\n",
                results$metrics$test["rmse"],
                results$metrics$test["mae"],
                results$metrics$test["r2"]))

    return(results)
  }

  # LEVEL 1: Train 6 XGBoost models to predict y_1, ..., y_6
  cat("\n=== LEVEL 1: Training First-Level Models ===\n")

  # Function to train a single first-level model
  train_level1_model <- function(Xy_data, train_idx, val_idx, model_name) {
    cat(sprintf("\nTraining %s...\n", model_name))

    # Prepare data
    X_train <- as.matrix(Xy_data[train_idx, -1])
    y_train <- Xy_data[train_idx, 1]
    X_val <- as.matrix(Xy_data[val_idx, -1])
    y_val <- Xy_data[val_idx, 1]

    # Create DMatrix
    dtrain <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
    dval <- xgboost::xgb.DMatrix(data = X_val, label = y_val)

    # Set hyperparameters
    params <- list(
      objective = "reg:squarederror",
      eta = 0.05,
      max_depth = 6,
      min_child_weight = 3,
      subsample = 0.8,
      colsample_bytree = 0.8
    )

    # Train with early stopping
    watchlist <- list(train = dtrain, val = dval)
    model <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = 500,
      watchlist = watchlist,
      early_stopping_rounds = 20,
      verbose = 0
    )

    cat(sprintf("  Best iteration: %d\n", model$best_iteration))
    cat(sprintf("  Train RMSE: %.6f\n", model$evaluation_log$train_rmse_mean[model$best_iteration]))
    cat(sprintf("  Val RMSE: %.6f\n", model$evaluation_log$val_rmse_mean[model$best_iteration]))

    return(model)
  }

  # Train all 6 first-level models
  models_level1 <- list()
  models_level1[[1]] <- train_level1_model(Xy1, train_indices, val_indices, "Model y_1")
  models_level1[[2]] <- train_level1_model(Xy2, train_indices, val_indices, "Model y_2")
  models_level1[[3]] <- train_level1_model(Xy3, train_indices, val_indices, "Model y_3")
  models_level1[[4]] <- train_level1_model(Xy4, train_indices, val_indices, "Model y_4")
  models_level1[[5]] <- train_level1_model(Xy5, train_indices, val_indices, "Model y_5")
  models_level1[[6]] <- train_level1_model(Xy6, train_indices, val_indices, "Model y_6")

  # Generate predictions from first-level models on all splits
  cat("\n=== Generating Level 1 Predictions ===\n")
  X_matrix <- as.matrix(X)
  dmatrix_all <- xgb.DMatrix(data = X_matrix)

  y_1_pred <- predict(models_level1[[1]], dmatrix_all)
  y_2_pred <- predict(models_level1[[2]], dmatrix_all)
  y_3_pred <- predict(models_level1[[3]], dmatrix_all)
  y_4_pred <- predict(models_level1[[4]], dmatrix_all)
  y_5_pred <- predict(models_level1[[5]], dmatrix_all)
  y_6_pred <- predict(models_level1[[6]], dmatrix_all)

  cat("Level 1 predictions generated for all data points\n")

  # LEVEL 2: Create stacked dataset with X + predictions
  cat("\n=== LEVEL 2: Preparing Stacked Dataset ===\n")
  X_stacked <- cbind(
    X,
    y_1_pred = y_1_pred,
    y_2_pred = y_2_pred,
    y_3_pred = y_3_pred,
    y_4_pred = y_4_pred,
    y_5_pred = y_5_pred,
    y_6_pred = y_6_pred
  )

  cat(sprintf("Stacked features: %d (original) + 6 (predictions) = %d total\n",
              ncol(X), ncol(X_stacked)))

  # Train final XGBoost model
  cat("\n=== Training Final Stacked Model ===\n")
  X_train_stacked <- as.matrix(X_stacked[train_indices, ])
  y_train <- fwd$y[train_indices]
  X_val_stacked <- as.matrix(X_stacked[val_indices, ])
  y_val <- fwd$y[val_indices]

  dtrain_final <- xgboost::xgb.DMatrix(data = X_train_stacked, label = y_train)
  dval_final <- xgboost::xgb.DMatrix(data = X_val_stacked, label = y_val)

  params_final <- list(
    objective = "reg:squarederror",
    eta = 0.05,
    max_depth = 6,
    min_child_weight = 3,
    subsample = 0.8,
    colsample_bytree = 0.8
  )

  watchlist_final <- list(train = dtrain_final, val = dval_final)
  model_final <- xgboost::xgb.train(
    params = params_final,
    data = dtrain_final,
    nrounds = 500,
    watchlist = watchlist_final,
    early_stopping_rounds = 20,
    verbose = 1
  )

  cat(sprintf("\nBest iteration: %d\n", model_final$best_iteration))

  # EVALUATION
  cat("\n=== Model Evaluation ===\n")

  # Predictions on all splits
  X_train_stacked_dm <- xgboost::xgb.DMatrix(data = X_train_stacked)
  X_val_stacked_dm <- xgboost::xgb.DMatrix(data = X_val_stacked)
  X_test_stacked <- as.matrix(X_stacked[test_indices, ])
  X_test_stacked_dm <- xgboost::xgb.DMatrix(data = X_test_stacked)

  pred_train <- predict(model_final, X_train_stacked_dm)
  pred_val <- predict(model_final, X_val_stacked_dm)
  pred_test <- predict(model_final, X_test_stacked_dm)

  y_test <- fwd$y[test_indices]

  # Calculate metrics
  rmse_train <- sqrt(mean((pred_train - y_train)^2))
  rmse_val <- sqrt(mean((pred_val - y_val)^2))
  rmse_test <- sqrt(mean((pred_test - y_test)^2))

  mae_train <- mean(abs(pred_train - y_train))
  mae_val <- mean(abs(pred_val - y_val))
  mae_test <- mean(abs(pred_test - y_test))

  r2_train <- 1 - sum((pred_train - y_train)^2) / sum((y_train - mean(y_train))^2)
  r2_val <- 1 - sum((pred_val - y_val)^2) / sum((y_val - mean(y_val))^2)
  r2_test <- 1 - sum((pred_test - y_test)^2) / sum((y_test - mean(y_test))^2)

  cat("\nPerformance Metrics:\n")
  cat(sprintf("Train - RMSE: %.6f, MAE: %.6f, R²: %.4f\n", rmse_train, mae_train, r2_train))
  cat(sprintf("Val   - RMSE: %.6f, MAE: %.6f, R²: %.4f\n", rmse_val, mae_val, r2_val))
  cat(sprintf("Test  - RMSE: %.6f, MAE: %.6f, R²: %.4f\n", rmse_test, mae_test, r2_test))

  # Feature importance
  cat("\n=== Feature Importance (Top 20) ===\n")
  importance_matrix <- xgboost::xgb.importance(model = model_final)
  print(head(importance_matrix, 20))

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

  # Save models and results
  if (is.null(cache)) {
    cat("No cache provided, skipping saving models.\n")
    return(results)
  }

  cat(sprintf("\nPersisting models to cache\n"))
  save_cache(cache, results)
  cat(sprintf("Models and results saved to cache: %s\n", cache$path))
  results
}
