library("devtools")

load_all()

library("tidyquant")
library("dplyr")
library("lubridate")
library("xts")
library("zoo")
library("rugarch")
library("ggplot2")
library("ggfortify")

# Try to load TensorFlow/Keras with error handling
tensorflow_available <- tryCatch({
  library("tensorflow")
  library("keras")
  TRUE
}, error = function(e) {
  cat("TensorFlow/Keras not available:", e$message, "\n")
  cat("Install with: install.packages(c('tensorflow', 'keras')); keras::install_keras()\n")
  FALSE
})

btc <- get_ticker("BSBTCUSDH1")
btc_series <- btc[, "adjusted"]

features <- build_features(btc_series)

y <- log(lead(btc[, "high"]) / btc[, "close"])
names(y) <- "y"

data <- merge(y, features, all = FALSE)

# ============================================================================
# LSTM MODEL TRAINING (DEEP LEARNING)
# ============================================================================

if (tensorflow_available) {
  
  cat("\n=== LSTM DEEP LEARNING MODEL TRAINING ===\n")
  
  # Prepare initial feature matrix and response vector
  data_clean <- data[!is.na(data$y), ]
  complete_cases <- complete.cases(data_clean)
  data_complete <- data_clean[complete_cases, ]
  
  cat("Original data points:", nrow(data), "\n")
  cat("After removing NA in target:", nrow(data_clean), "\n") 
  cat("Complete cases:", nrow(data_complete), "\n")
  
  if (nrow(data_complete) > 0) {
    # Extract response variable
    y_vec <- as.numeric(data_complete$y)
    
    # Create feature matrix (exclude the response variable)
    feature_data <- data_complete[, !colnames(data_complete) %in% "y"]
    
    # Convert factor variables to dummy variables using model.matrix
    factor_cols <- sapply(feature_data, is.factor)
    
    if (any(factor_cols)) {
      formula_str <- paste("~", paste(colnames(feature_data), collapse = " + "))
      X_matrix <- model.matrix(as.formula(formula_str), data = as.data.frame(feature_data))[, -1]
    } else {
      X_matrix <- as.matrix(feature_data)
    }
    
    # Remove any columns with zero variance
    col_vars <- apply(X_matrix, 2, var, na.rm = TRUE)
    X_matrix <- X_matrix[, col_vars > 0, drop = FALSE]
    
    cat("Base feature matrix dimensions:", dim(X_matrix), "\n")
    cat("Base features included:", colnames(X_matrix), "\n")
    
    # LSTM-specific data preparation
    create_lstm_sequences <- function(X_matrix, y_vec, sequence_length = 60) {
      cat("Creating LSTM sequences with length:", sequence_length, "\n")
      
      n_samples <- nrow(X_matrix) - sequence_length + 1
      n_features <- ncol(X_matrix)
      
      # Create 3D array for LSTM input: (samples, timesteps, features)
      X_sequences <- array(0, dim = c(n_samples, sequence_length, n_features))
      y_sequences <- numeric(n_samples)
      
      for (i in 1:n_samples) {
        X_sequences[i, , ] <- X_matrix[i:(i + sequence_length - 1), ]
        y_sequences[i] <- y_vec[i + sequence_length - 1]
      }
      
      cat("LSTM sequences created. Shape:", dim(X_sequences), "\n")
      return(list(X = X_sequences, y = y_sequences))
    }
    
    # Create sequences for LSTM
    sequence_length <- 60  # Use 60 time steps (hours) for prediction
    lstm_data <- create_lstm_sequences(X_matrix, y_vec, sequence_length)
    X_lstm <- lstm_data$X
    y_lstm <- lstm_data$y
    
    # Normalize features for better LSTM performance
    cat("Normalizing features for LSTM...\n")
    
    # Calculate normalization parameters from training data only
    n_total <- dim(X_lstm)[1]
    train_size <- floor(0.7 * n_total)
    val_size <- floor(0.15 * n_total)
    
    # Get training data for normalization
    X_train_norm <- X_lstm[1:train_size, , ]
    
    # Calculate mean and std for each feature across all samples and timesteps
    feature_means <- apply(X_train_norm, 3, mean, na.rm = TRUE)
    feature_sds <- apply(X_train_norm, 3, sd, na.rm = TRUE)
    
    # Avoid division by zero
    feature_sds[feature_sds == 0] <- 1
    
    # Normalize all data using training statistics
    for (i in 1:dim(X_lstm)[3]) {
      X_lstm[, , i] <- (X_lstm[, , i] - feature_means[i]) / feature_sds[i]
    }
    
    # Time series aware train/validation/test split (chronological)
    cat("Creating time series aware train/validation/test split...\n")
    
    train_indices <- 1:train_size
    val_indices <- (train_size + 1):(train_size + val_size)
    test_indices <- (train_size + val_size + 1):n_total
    
    X_train <- X_lstm[train_indices, , ]
    X_val <- X_lstm[val_indices, , ]
    X_test <- X_lstm[test_indices, , ]
    y_train <- y_lstm[train_indices]
    y_val <- y_lstm[val_indices]
    y_test <- y_lstm[test_indices]
    
    cat("Training set size:", length(y_train), "\n")
    cat("Validation set size:", length(y_val), "\n")
    cat("Test set size:", length(y_test), "\n")
    
    # Build LSTM model architecture
    cat("\n=== BUILDING LSTM ARCHITECTURE ===\n")
    
    model <- keras_model_sequential() %>%
      layer_lstm(units = 128, return_sequences = TRUE, input_shape = c(sequence_length, dim(X_lstm)[3])) %>%
      layer_dropout(rate = 0.2) %>%
      layer_lstm(units = 64, return_sequences = TRUE) %>%
      layer_dropout(rate = 0.2) %>%
      layer_lstm(units = 32, return_sequences = FALSE) %>%
      layer_dropout(rate = 0.2) %>%
      layer_dense(units = 16, activation = 'relu') %>%
      layer_dropout(rate = 0.1) %>%
      layer_dense(units = 1, activation = 'linear')
    
    # Compile model with advanced optimizer
    model %>% compile(
      optimizer = optimizer_adam(learning_rate = 0.001),
      loss = 'mse',
      metrics = c('mae')
    )
    
    # Print model summary
    cat("LSTM Model Architecture:\n")
    summary(model)
    
    # Define callbacks for training
    callbacks_list <- list(
      callback_early_stopping(
        monitor = 'val_loss',
        patience = 15,
        restore_best_weights = TRUE
      ),
      callback_reduce_lr_on_plateau(
        monitor = 'val_loss',
        factor = 0.5,
        patience = 8,
        min_lr = 1e-7
      )
    )
    
    # Train the LSTM model
    cat("\n=== TRAINING LSTM MODEL ===\n")
    
    history <- model %>% fit(
      x = X_train,
      y = y_train,
      batch_size = 32,
      epochs = 100,
      validation_data = list(X_val, y_val),
      callbacks = callbacks_list,
      verbose = 1
    )
    
    # Make predictions on all sets
    cat("\n=== GENERATING PREDICTIONS ===\n")
    
    lstm_pred_train <- model %>% predict(X_train)
    lstm_pred_val <- model %>% predict(X_val)
    lstm_pred_test <- model %>% predict(X_test)
    
    # Convert to vectors
    lstm_pred_train <- as.vector(lstm_pred_train)
    lstm_pred_val <- as.vector(lstm_pred_val)
    lstm_pred_test <- as.vector(lstm_pred_test)
    
    # Calculate comprehensive performance metrics
    calculate_metrics <- function(actual, predicted, set_name) {
      rmse <- sqrt(mean((actual - predicted)^2))
      mae <- mean(abs(actual - predicted))
      r2 <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
      
      # Directional accuracy
      actual_direction <- sign(actual)
      pred_direction <- sign(predicted)
      directional_accuracy <- mean(actual_direction == pred_direction)
      
      return(list(
        set = set_name,
        rmse = rmse,
        mae = mae,
        r2 = r2,
        directional_accuracy = directional_accuracy
      ))
    }
    
    train_metrics <- calculate_metrics(y_train, lstm_pred_train, "Training")
    val_metrics <- calculate_metrics(y_val, lstm_pred_val, "Validation")
    test_metrics <- calculate_metrics(y_test, lstm_pred_test, "Test")
    
    # Print comprehensive results
    cat("\n=== LSTM MODEL RESULTS ===\n")
    for (metrics in list(train_metrics, val_metrics, test_metrics)) {
      cat(sprintf("%s Set:\n", metrics$set))
      cat(sprintf("  RMSE: %.6f\n", metrics$rmse))
      cat(sprintf("  MAE: %.6f\n", metrics$mae))
      cat(sprintf("  R²: %.4f\n", metrics$r2))
      cat(sprintf("  Directional Accuracy: %.2f%%\n", metrics$directional_accuracy * 100))
      cat("\n")
    }
    
    # Create enhanced visualizations
    cat("\n=== GENERATING LSTM PLOTS ===\n")
    
    # Plot 1: Training history
    png("lstm_training_history.png", width = 1200, height = 800)
    par(mfrow = c(2, 2))
    
    # Loss curves
    epochs <- 1:length(history$metrics$loss)
    plot(epochs, history$metrics$loss, type = "l", col = "blue", lwd = 2,
         main = "Model Loss", xlab = "Epoch", ylab = "Loss")
    lines(epochs, history$metrics$val_loss, col = "red", lwd = 2)
    legend("topright", legend = c("Training", "Validation"), col = c("blue", "red"), lty = 1)
    
    # MAE curves
    plot(epochs, history$metrics$mae, type = "l", col = "blue", lwd = 2,
         main = "Model MAE", xlab = "Epoch", ylab = "MAE")
    lines(epochs, history$metrics$val_mae, col = "red", lwd = 2)
    legend("topright", legend = c("Training", "Validation"), col = c("blue", "red"), lty = 1)
    
    # Learning rate (if available)
    if ("lr" %in% names(history$metrics)) {
      plot(epochs, history$metrics$lr, type = "l", col = "green", lwd = 2,
           main = "Learning Rate", xlab = "Epoch", ylab = "Learning Rate")
    } else {
      plot(1, 1, type = "n", main = "Learning Rate Schedule", xlab = "Epoch", ylab = "LR")
      text(1, 1, "Learning rate schedule\nnot recorded", cex = 1.2)
    }
    
    # Model architecture visualization
    plot(1, 1, type = "n", main = "LSTM Architecture", xlab = "", ylab = "")
    text(1, 1, paste("LSTM Model:\n",
                     "- Input: (", sequence_length, ",", dim(X_lstm)[3], ")\n",
                     "- LSTM Layers: 128→64→32\n",
                     "- Dense Layers: 16→1\n",
                     "- Dropout: 0.1-0.2\n",
                     "- Optimizer: Adam"), cex = 0.9)
    
    dev.off()
    cat("Saved: lstm_training_history.png\n")
    
    # Plot 2: Predictions analysis
    png("lstm_predictions_analysis.png", width = 1200, height = 800)
    par(mfrow = c(2, 2))
    
    # Validation predictions
    plot(y_val, lstm_pred_val, main = paste("Validation: Actual vs Predicted\nR² =", round(val_metrics$r2, 4)),
         xlab = "Actual", ylab = "Predicted", col = "blue", pch = 16, alpha = 0.6)
    abline(0, 1, col = "red", lwd = 2)
    
    # Test predictions  
    plot(y_test, lstm_pred_test, main = paste("Test: Actual vs Predicted\nR² =", round(test_metrics$r2, 4)),
         xlab = "Actual", ylab = "Predicted", col = "green", pch = 16, alpha = 0.6)
    abline(0, 1, col = "red", lwd = 2)
    
    # Residuals analysis
    val_residuals <- y_val - lstm_pred_val
    plot(lstm_pred_val, val_residuals, main = "Validation: Residuals vs Fitted",
         xlab = "Fitted Values", ylab = "Residuals", col = "blue", pch = 16, alpha = 0.6)
    abline(h = 0, col = "red", lwd = 2)
    
    # QQ plot of residuals
    qqnorm(val_residuals, main = "Q-Q Plot: Validation Residuals")
    qqline(val_residuals, col = "red", lwd = 2)
    
    dev.off()
    cat("Saved: lstm_predictions_analysis.png\n")
    
    # Plot 3: Time series analysis
    png("lstm_timeseries_analysis.png", width = 1200, height = 600)
    
    # Create time index for plotting
    time_idx <- 1:length(y_lstm)
    train_time <- time_idx[train_indices]
    val_time <- time_idx[val_indices]
    test_time <- time_idx[test_indices]
    
    plot(time_idx, y_lstm, type = "l", col = "black", lwd = 1,
         main = "LSTM Predictions vs Actual (Time Series)",
         xlab = "Time Index", ylab = "Log Returns")
    lines(train_time, lstm_pred_train, col = "blue", lwd = 1)
    lines(val_time, lstm_pred_val, col = "red", lwd = 1)
    lines(test_time, lstm_pred_test, col = "green", lwd = 1)
    
    # Add vertical lines to separate train/val/test
    abline(v = max(train_time), col = "gray", lty = 2)
    abline(v = max(val_time), col = "gray", lty = 2)
    
    legend("topright", legend = c("Actual", "Train Pred", "Val Pred", "Test Pred"),
           col = c("black", "blue", "red", "green"), lty = 1, lwd = c(1,1,1,1))
    
    dev.off()
    cat("Saved: lstm_timeseries_analysis.png\n")
    
    # Plot 4: Feature importance approximation using permutation
    cat("Computing feature importance via permutation...\n")
    
    # Simple feature importance by permutation (approximate)
    baseline_loss <- mean((y_val - lstm_pred_val)^2)
    feature_importance <- numeric(dim(X_lstm)[3])
    names(feature_importance) <- colnames(X_matrix)
    
    for (i in 1:min(10, dim(X_lstm)[3])) {  # Limit to top 10 features for performance
      # Create permuted validation set
      X_val_perm <- X_val
      X_val_perm[, , i] <- sample(X_val_perm[, , i])
      
      # Get predictions with permuted feature
      perm_pred <- model %>% predict(X_val_perm)
      perm_loss <- mean((y_val - as.vector(perm_pred))^2)
      
      # Importance is increase in loss
      feature_importance[i] <- perm_loss - baseline_loss
    }
    
    # Plot feature importance
    png("lstm_feature_importance.png", width = 1000, height = 600)
    
    # Get top features
    top_features <- head(sort(feature_importance, decreasing = TRUE), 10)
    
    barplot(top_features, 
            main = "LSTM Feature Importance (Permutation)",
            xlab = "Features", ylab = "Importance (Loss Increase)",
            las = 2, cex.names = 0.8, col = "steelblue")
    
    dev.off()
    cat("Saved: lstm_feature_importance.png\n")
    
    # Store LSTM results
    lstm_results <- list(
      model = model,
      history = history,
      predictions_train = lstm_pred_train,
      predictions_val = lstm_pred_val,
      predictions_test = lstm_pred_test,
      metrics_train = train_metrics,
      metrics_val = val_metrics,
      metrics_test = test_metrics,
      feature_importance = feature_importance,
      sequence_length = sequence_length,
      train_indices = train_indices,
      val_indices = val_indices,
      test_indices = test_indices,
      feature_names = colnames(X_matrix),
      normalization_params = list(means = feature_means, sds = feature_sds)
    )
    
    # Enhanced summary statistics
    cat("\n=== LSTM MODEL SUMMARY ===\n")
    cat("Model Performance Summary:\n")
    cat(sprintf("- Validation RMSE: %.6f\n", val_metrics$rmse))
    cat(sprintf("- Test Set R²: %.4f\n", test_metrics$r2))
    cat(sprintf("- Test Set Directional Accuracy: %.2f%%\n", test_metrics$directional_accuracy * 100))
    cat(sprintf("- Sequence Length: %d hours\n", sequence_length))
    cat(sprintf("- Number of Features: %d\n", dim(X_lstm)[3]))
    cat(sprintf("- Training Epochs: %d\n", length(history$metrics$loss)))
    
    cat("\nModel Architecture:\n")
    cat("- LSTM Layers: 128 → 64 → 32 units\n")
    cat("- Dense Layers: 16 → 1 units\n")
    cat("- Dropout Rates: 0.1 - 0.2\n")
    cat("- Optimizer: Adam (lr=0.001)\n")
    cat("- Early Stopping: Enabled\n")
    
    cat("\nTop 5 Most Important Features (Permutation):\n")
    top_5_features <- head(sort(feature_importance, decreasing = TRUE), 5)
    for (i in 1:length(top_5_features)) {
      cat(sprintf("%d. %s (Importance: %.6f)\n", i, names(top_5_features)[i], top_5_features[i]))
    }
    
    cat("\nModel Generalization Analysis:\n")
    cat(sprintf("- Train R²: %.4f\n", train_metrics$r2))
    cat(sprintf("- Validation R²: %.4f\n", val_metrics$r2))
    cat(sprintf("- Test R²: %.4f\n", test_metrics$r2))
    overfitting_score <- train_metrics$r2 - test_metrics$r2
    cat(sprintf("- Overfitting Score: %.4f %s\n", overfitting_score, 
                ifelse(overfitting_score > 0.1, "(High)", ifelse(overfitting_score > 0.05, "(Moderate)", "(Low)"))))
    
    cat("\nDeep Learning Advantages:\n")
    cat("- Captures non-linear temporal patterns\n")
    cat("- Learns complex feature interactions automatically\n")
    cat("- Handles sequential dependencies in time series\n")
    cat("- Robust to noise through dropout regularization\n")
    
  } else {
    cat("No complete cases available for LSTM modeling!\n")
  }
  
} else {
  cat("\n=== LSTM MODEL TRAINING ===\n")
  cat("TensorFlow/Keras not available. Skipping LSTM analysis.\n")
  cat("To install TensorFlow/Keras:\n")
  cat("1. install.packages(c('tensorflow', 'keras'))\n")
  cat("2. keras::install_keras()\n")
  cat("3. Restart R session\n")
}
