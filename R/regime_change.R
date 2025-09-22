rcfit <- function(series, regimes) {
  # Handle different input types and convert to xts if needed
  if (is.data.frame(series)) {
    # Look for common price column names
    price_cols <- c("adjusted", "close", "price", "value")
    found_col <- NULL
    for (col in price_cols) {
      if (col %in% names(series)) {
        found_col <- col
        break
      }
    }

    if (!is.null(found_col)) {
      if ("date" %in% names(series)) {
        series_xts <- xts::xts(series[[found_col]], order.by = as.Date(series$date))
      } else {
        # Use row indices as dates
        series_xts <- xts::xts(series[[found_col]], order.by = seq_len(nrow(series)))
      }
    } else {
      stop("Data frame must contain a price column ('adjusted', 'close', 'price', or 'value')")
    }
  } else if (xts::is.xts(series)) {
    series_xts <- series
  } else if (is.vector(series)) {
    series_xts <- xts::xts(series, order.by = as.Date("2020-01-01") + seq_len(length(series)) - 1)
  } else {
    stop("Input must be a data frame, xts object, or vector")
  }

  # Remove NA values
  series_clean <- series_xts[!is.na(series_xts)]

  if (length(series_clean) < 10) {
    stop("Insufficient data for regime change analysis (need at least 10 observations)")
  }

  # Convert to numeric vector for analysis
  y <- as.numeric(series_clean)
  n <- length(y)

  if (regimes < 2 || regimes > n/3) {
    stop("Number of regimes must be between 2 and n/3")
  }

  # Calculate returns for regime detection - handle negative/zero values
  y_positive <- pmax(y, 1e-8)  # Ensure positive values for log
  returns <- diff(log(y_positive))
  returns <- c(0, returns)  # Add first return as 0
  
  # Replace any infinite or NaN values
  returns[!is.finite(returns)] <- 0
  
  # Calculate rolling statistics for regime identification
  window_size <- max(10, floor(n/20))  # Adaptive window size
  
  # Rolling volatility (standard deviation of returns)
  rolling_vol <- zoo::rollapply(returns, width = window_size, FUN = sd, 
                               fill = NA, align = "center")
  
  # Rolling mean return
  rolling_mean <- zoo::rollapply(returns, width = window_size, FUN = mean, 
                                fill = NA, align = "center")
  
  # Fill NAs at the beginning and end
  rolling_vol[is.na(rolling_vol)] <- mean(rolling_vol, na.rm = TRUE)
  rolling_mean[is.na(rolling_mean)] <- mean(rolling_mean, na.rm = TRUE)
  
  # Use k-means clustering on the feature space to identify regimes
  features <- cbind(rolling_vol, rolling_mean)
  
  # Remove any rows with NaN or infinite values
  valid_rows <- is.finite(features[,1]) & is.finite(features[,2])
  if (sum(valid_rows) < regimes) {
    # Fallback to simple quantile-based breaks if not enough valid data
    break_spacing <- floor(n / regimes)
    break_points <- seq(break_spacing, n - break_spacing, by = break_spacing)[1:(regimes-1)]
    regime_indicators <- rep(1:regimes, each = break_spacing, length.out = n)
    kmeans_result <- NULL
    features_scaled <- features
  } else {
    # Standardize features
    features_scaled <- scale(features)
    
    # Check for distinct data points
    unique_features <- unique(features_scaled[valid_rows, , drop = FALSE])
    n_unique <- nrow(unique_features)
    
    if (n_unique < regimes) {
      # If not enough unique points, reduce number of regimes or use simple breaks
      actual_regimes <- min(regimes, n_unique, max(2, floor(n/10)))
      if (actual_regimes < 2) {
        # Fallback to simple time-based breaks
        break_spacing <- floor(n / regimes)
        break_points <- seq(break_spacing, n - break_spacing, by = break_spacing)[1:(regimes-1)]
        regime_indicators <- rep(1:regimes, each = break_spacing, length.out = n)
        kmeans_result <- NULL
      } else {
        # Apply k-means clustering with reduced regimes
        set.seed(123)  # For reproducibility
        kmeans_result <- kmeans(features_scaled[valid_rows, , drop = FALSE], 
                               centers = actual_regimes, nstart = 25)
        
        # Map results back to full series
        regime_indicators <- rep(1, n)
        regime_indicators[valid_rows] <- kmeans_result$cluster
        
        # Pad regime numbers to match requested regimes if needed
        if (actual_regimes < regimes) {
          # Simple time-based assignment for remaining regimes
          break_spacing <- floor(n / regimes)
          regime_indicators <- rep(1:regimes, each = break_spacing, length.out = n)
        }
      }
    } else {
      # Apply k-means clustering
      set.seed(123)  # For reproducibility
      kmeans_result <- kmeans(features_scaled[valid_rows, , drop = FALSE], 
                             centers = regimes, nstart = 25)
      
      # Map results back to full series
      regime_indicators <- rep(1, n)
      regime_indicators[valid_rows] <- kmeans_result$cluster
    }
  }
  
  # Smooth regime transitions to avoid too frequent switching
  # Apply a simple majority filter
  filter_window <- max(3, floor(window_size/3))
  regime_indicators_smooth <- regime_indicators
  
  for (i in (filter_window + 1):(n - filter_window)) {
    window_regimes <- regime_indicators[(i - filter_window):(i + filter_window)]
    regime_indicators_smooth[i] <- as.numeric(names(sort(table(window_regimes), decreasing = TRUE))[1])
  }
  
  # Find break points from regime changes
  regime_changes <- which(diff(regime_indicators_smooth) != 0)
  break_points <- regime_changes
  
  # Fit models for each regime
  regime_models <- list()
  fitted_values <- numeric(n)
  residuals_all <- numeric(n)
  
  # Get regime boundaries
  regime_boundaries <- c(0, break_points, n)
  
  for (i in 1:(length(regime_boundaries)-1)) {
    start_idx <- regime_boundaries[i] + 1
    end_idx <- regime_boundaries[i+1]
    
    regime_data <- y[start_idx:end_idx]
    regime_time <- seq_len(length(regime_data))
    
    # Fit different models based on regime characteristics
    regime_num <- regime_indicators_smooth[start_idx]
    
    # Determine model type based on regime characteristics
    regime_returns <- returns[start_idx:end_idx]
    regime_vol <- sd(regime_returns, na.rm = TRUE)
    regime_trend <- mean(regime_returns, na.rm = TRUE)
    
    if (abs(regime_trend) > 0.001) {
      # Trending regime - use linear model
      model <- lm(regime_data ~ regime_time)
    } else {
      # Mean-reverting regime - use constant mean model
      model <- lm(regime_data ~ 1)
    }
    
    regime_models[[i]] <- model
    
    # Store fitted values and residuals
    fitted_values[start_idx:end_idx] <- fitted(model)
    residuals_all[start_idx:end_idx] <- residuals(model)
  }
  
  # Calculate total sum of squared residuals
  total_ssr <- sum(residuals_all^2)
  
  # Convert break points to original time index
  original_dates <- zoo::index(series_clean)
  break_dates <- if (length(break_points) > 0) original_dates[break_points] else NULL
  
  # Create fitted object
  fit <- list(
    original_series = series_clean,
    fitted_values = xts::xts(fitted_values, order.by = original_dates),
    residuals = xts::xts(residuals_all, order.by = original_dates),
    regime_indicators = xts::xts(regime_indicators_smooth, order.by = original_dates),
    break_points = break_points,
    break_dates = break_dates,
    regime_models = regime_models,
    n_regimes = regimes,
    ssr = total_ssr,
    features = features_scaled,
    kmeans_result = kmeans_result
  )

  class(fit) <- "rcfit"
  fit
}

rcfitted <- function(fit) {
  if (!inherits(fit, "rcfit")) {
    stop("Input must be a rcfit object")
  }

  # Return fitted values with regime indicators
  result <- merge(fit$fitted_values, fit$regime_indicators)
  colnames(result) <- c("fitted", "regime")

  result
}

rcpredict <- function(fit, n_ahead = 3) {
  if (!inherits(fit, "rcfit")) {
    stop("Input must be a rcfit object")
  }

  if (n_ahead < 1) {
    stop("n_ahead must be at least 1")
  }

  # Get the last regime and its characteristics
  last_regime <- as.numeric(fit$regime_indicators[nrow(fit$regime_indicators)])
  
  # Find the model for the last regime
  regime_boundaries <- c(0, fit$break_points, length(fit$regime_indicators))
  last_regime_start <- tail(regime_boundaries, 2)[1] + 1
  last_regime_model_idx <- length(fit$regime_models)
  
  last_model <- fit$regime_models[[last_regime_model_idx]]
  
  # Get the length of data in the last regime
  regime_data_length <- length(fit$regime_indicators) - last_regime_start + 1
  
  # Generate predictions using the last regime's model
  if (length(coef(last_model)) > 1) {
    # Linear model with trend
    future_time <- (regime_data_length + 1):(regime_data_length + n_ahead)
    predictions <- predict(last_model, newdata = data.frame(regime_time = future_time))
  } else {
    # Constant mean model
    predictions <- rep(coef(last_model)[1], n_ahead)
  }
  
  # Add some uncertainty based on recent volatility
  recent_residuals <- tail(as.numeric(fit$residuals), min(20, length(fit$residuals)))
  prediction_sd <- sd(recent_residuals, na.rm = TRUE)
  
  # Create future dates
  last_date <- zoo::index(fit$original_series)[length(fit$original_series)]

  if (inherits(last_date, "Date")) {
    future_dates <- seq(from = last_date + 1, length.out = n_ahead, by = "day")
  } else {
    future_dates <- (as.numeric(last_date) + 1):(as.numeric(last_date) + n_ahead)
  }

  # Create prediction xts object
  predictions_xts <- xts::xts(predictions, order.by = future_dates)
  colnames(predictions_xts) <- "predicted"

  # Add regime indicator (assume last regime continues)
  regime_xts <- xts::xts(rep(last_regime, n_ahead), order.by = future_dates)
  colnames(regime_xts) <- "regime"
  
  # Add prediction intervals
  lower_bound <- predictions - 1.96 * prediction_sd
  upper_bound <- predictions + 1.96 * prediction_sd
  
  lower_xts <- xts::xts(lower_bound, order.by = future_dates)
  upper_xts <- xts::xts(upper_bound, order.by = future_dates)
  colnames(lower_xts) <- "lower_95"
  colnames(upper_xts) <- "upper_95"

  # Combine predictions with regime indicators and intervals
  result <- merge(predictions_xts, regime_xts, lower_xts, upper_xts)

  result
}
