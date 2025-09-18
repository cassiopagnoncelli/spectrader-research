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
    series_xts <- xts::xts(series, order.by = seq_len(length(series)))
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

  # Create time index
  time_index <- 1:n

  # Simple structural break detection using recursive residuals approach
  # This is a basic implementation - in practice you'd use strucchange package

  if (regimes < 2 || regimes > n/3) {
    stop("Number of regimes must be between 2 and n/3")
  }

  # Find optimal break points using sum of squared residuals
  best_breaks <- NULL
  best_ssr <- Inf

  # For simplicity, use quantile-based initial break points
  if (regimes == 2) {
    # Single break point
    possible_breaks <- seq(floor(n*0.2), floor(n*0.8), by = 1)

    for (bp in possible_breaks) {
      # Fit two separate linear models
      regime1_data <- y[1:bp]
      regime2_data <- y[(bp+1):n]

      if (length(regime1_data) < 3 || length(regime2_data) < 3) next

      # Simple linear trend models for each regime
      t1 <- seq_len(length(regime1_data))
      t2 <- seq_len(length(regime2_data))

      model1 <- lm(regime1_data ~ t1)
      model2 <- lm(regime2_data ~ t2)

      ssr <- sum(residuals(model1)^2) + sum(residuals(model2)^2)

      if (ssr < best_ssr) {
        best_ssr <- ssr
        best_breaks <- bp
      }
    }
  } else {
    # Multiple break points - use equal spacing as approximation
    break_spacing <- floor(n / regimes)
    best_breaks <- seq(break_spacing, n - break_spacing, by = break_spacing)[1:(regimes-1)]
  }

  # Fit models for each regime
  break_points <- c(0, best_breaks, n)
  regime_models <- list()
  fitted_values <- numeric(n)
  residuals_all <- numeric(n)

  for (i in 1:(length(break_points)-1)) {
    start_idx <- break_points[i] + 1
    end_idx <- break_points[i+1]

    regime_data <- y[start_idx:end_idx]
    regime_time <- seq_len(length(regime_data))

    # Fit linear trend model for this regime
    model <- lm(regime_data ~ regime_time)
    regime_models[[i]] <- model

    # Store fitted values and residuals
    fitted_values[start_idx:end_idx] <- fitted(model)
    residuals_all[start_idx:end_idx] <- residuals(model)
  }

  # Create regime indicators
  regime_indicators <- numeric(n)
  for (i in 1:(length(break_points)-1)) {
    start_idx <- break_points[i] + 1
    end_idx <- break_points[i+1]
    regime_indicators[start_idx:end_idx] <- i
  }

  # Convert break points to original time index
  original_dates <- zoo::index(series_clean)
  break_dates <- original_dates[best_breaks]

  # Create fitted object
  fit <- list(
    original_series = series_clean,
    fitted_values = xts::xts(fitted_values, order.by = original_dates),
    residuals = xts::xts(residuals_all, order.by = original_dates),
    regime_indicators = xts::xts(regime_indicators, order.by = original_dates),
    break_points = best_breaks,
    break_dates = break_dates,
    regime_models = regime_models,
    n_regimes = regimes,
    ssr = best_ssr
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

  # Get the last regime
  last_regime <- as.numeric(fit$regime_indicators[nrow(fit$regime_indicators)])
  last_model <- fit$regime_models[[last_regime]]

  # Get the last time point in the regime
  regime_data_length <- sum(fit$regime_indicators == last_regime)

  # Generate predictions using the last regime's model
  future_time <- (regime_data_length + 1):(regime_data_length + n_ahead)
  predictions <- predict(last_model, newdata = data.frame(regime_time = future_time))

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

  # Add regime indicator
  regime_xts <- xts::xts(rep(last_regime, n_ahead), order.by = future_dates)
  colnames(regime_xts) <- "regime"

  # Combine predictions with regime indicators
  result <- merge(predictions_xts, regime_xts)

  result
}
