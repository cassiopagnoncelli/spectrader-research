calcreturns <- function(series, log = FALSE) {
  if (log) {
    return(diff(log(series)) / lag(log(series), 1))
  }
  diff(series) / lag(series, 1)
}

withexovars <- function(series) {
  if (!inherits(series, "xts"))
    stop("series should be an xts object")

  # Fetch common exogenous variables for financial time series modeling.
  vix <- get_ticker("VIX")[, "close"]
  sp500 <- get_ticker("SP500") %>% calcreturns
  dxy <- get_ticker("DXYLOOKALIKE")
  cryptocap <- get_ticker("CGMCAP") %>% log
  cryptogrowth <- get_ticker("CGMCAP") %>% calcreturns
  bonds <- get_ticker("DGS2") %>% calcreturns
  jobs <- get_ticker("IHLIDXUS") %>% calcreturns
  oil <- get_ticker("DCOILWTICO") %>% calcreturns
  goldvol <- get_ticker("GVZCLS")

  # Set proper column names for standard variables
  colnames(vix) <- "vix"
  colnames(sp500) <- "sp500_returns"
  colnames(dxy) <- "dxy"
  colnames(cryptocap) <- "crypto_cap_log"
  colnames(cryptogrowth) <- "crypto_returns"
  colnames(bonds) <- "bonds_returns"
  colnames(jobs) <- "jobs_returns"
  colnames(oil) <- "oil_returns"
  colnames(goldvol) <- "gold_vol"

  # Create the standard exogenous variables list
  stdseries <- cbind(
    # vix = vix,
    # sp500 = sp500,
    dxy = dxy,
    # cryptocap = cryptocap,
    cryptogrowth = cryptogrowth
    # bonds = bonds,
    # jobs = jobs,
    # oil = oil,
    # goldvol = goldvol
  )

  # Merged user series with exogenous variables
  merged <- merge(series, stdseries, join = "left", fill = NA)

  # Carry forward the last observation to fill NAs
  carried_forward <- na.locf(merged)
  carried_forward
}

garchvar <- function(series, n.ahead = 0) {
  # Handle different input types
  if (is.data.frame(series)) {
    # If it's a data frame (like from tq_get), extract the adjusted close prices
    if ("adjusted" %in% names(series)) {
      price_series <- series$adjusted
      dates <- series$date
    } else if ("close" %in% names(series)) {
      price_series <- series$close
      dates <- series$date
    } else {
      stop("Data frame must contain 'adjusted' or 'close' column")
    }

    # Calculate returns manually
    returns <- diff(price_series) / price_series[-length(price_series)]
    returns_dates <- dates[-1]  # Remove first date since we lose one observation

    # Create xts object
    series_returns_xts <- xts::xts(returns, order.by = returns_dates)

  } else if (xts::is.xts(series)) {
    # If it's already an xts object, calculate returns
    # Handle multi-column xts objects by using the first column
    if (ncol(series) > 1) {
      series <- series[, 1]
    }

    series_returns_xts <- diff(series) / lag(series, 1)
    # Use na.omit instead of logical indexing to avoid xts indexing issues
    series_returns_xts <- na.omit(series_returns_xts)

  } else {
    stop("Input must be a data frame with date/price columns or an xts object")
  }

  # Remove any remaining NA values using na.omit for xts compatibility
  series_returns_xts <- na.omit(series_returns_xts)

  # Additional data validation to remove infinite and NaN values
  # Check for infinite values
  if (any(is.infinite(as.numeric(series_returns_xts)))) {
    warning("Infinite values detected in returns, removing them")
    series_returns_xts <- series_returns_xts[is.finite(as.numeric(series_returns_xts))]
  }

  # Check for NaN values
  if (any(is.nan(as.numeric(series_returns_xts)))) {
    warning("NaN values detected in returns, removing them")
    series_returns_xts <- series_returns_xts[!is.nan(as.numeric(series_returns_xts))]
  }

  # Remove extreme outliers that might cause numerical issues
  returns_numeric <- as.numeric(series_returns_xts)
  q99 <- quantile(returns_numeric, 0.99, na.rm = TRUE)
  q01 <- quantile(returns_numeric, 0.01, na.rm = TRUE)

  # Cap extreme values at 99th and 1st percentiles
  outlier_indices <- which(returns_numeric > q99 | returns_numeric < q01)
  if (length(outlier_indices) > 0) {
    warning(paste("Capping", length(outlier_indices), "extreme outliers"))
    returns_numeric[returns_numeric > q99] <- q99
    returns_numeric[returns_numeric < q01] <- q01
    series_returns_xts <- xts::xts(returns_numeric, order.by = zoo::index(series_returns_xts))
  }

  # Check if we have enough data after cleaning
  if (length(series_returns_xts) < 10) {
    stop("Insufficient data for GARCH modeling after data cleaning (need at least 10 observations)")
  }

  # Final check for any remaining problematic values
  if (any(!is.finite(as.numeric(series_returns_xts)))) {
    stop("Data still contains non-finite values after cleaning. Cannot proceed with GARCH modeling.")
  }

  # Specify GARCH(1,1) model
  garch_spec <- rugarch::ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
    distribution.model = "norm"
  )

  # Fit GARCH model with error handling
  tryCatch({
    garch_fit <- rugarch::ugarchfit(spec = garch_spec, data = series_returns_xts)

    # Return conditional volatility (sigma), not fitted values (mu)
    # fitted() returns the conditional mean, sigma() returns conditional volatility
    volatility_values <- rugarch::sigma(garch_fit)
    colnames(volatility_values) <- "volatility"
    return(volatility_values)
  }, error = function(e) {
    stop(paste("GARCH model fitting failed:", e$message,
               "\nData summary: min =", min(series_returns_xts),
               ", max =", max(series_returns_xts),
               ", length =", length(series_returns_xts)))
  })
}

tafeatures <- function(series, slow = 200, long = 80, short = 20, signal = 8, as.xts = TRUE) {
  # Handle different input types first
  if (is.list(series)) {
    # If it's a list, try to extract the first element or find a price column
    if (length(series) == 1) {
      series <- series[[1]]
    } else {
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
        series <- series[[found_col]]
      } else {
        stop("Cannot identify price column in list. Expected 'adjusted', 'close', 'price', or 'value'")
      }
    }
  }

  # Store original index for later use
  if (xts::is.xts(series)) {
    original_index <- zoo::index(series)

    # Handle multi-column xts objects by selecting the first column
    if (ncol(series) > 1) {
      # Look for common price column names first
      price_cols <- c("adjusted", "close", "price", "value")
      found_col <- NULL
      for (col in price_cols) {
        if (col %in% colnames(series)) {
          found_col <- col
          break
        }
      }

      if (!is.null(found_col)) {
        series_values <- as.numeric(series[, found_col])
      } else {
        # Use the first column if no standard price column found
        series_values <- as.numeric(series[, 1])
      }
    } else {
      # Single column xts object
      series_values <- as.numeric(series)
    }

    original_length <- length(series_values)  # Use length of extracted values
  } else if (is.data.frame(series)) {
    # Handle data frame input
    original_length <- nrow(series)
    original_index <- seq_len(original_length)

    # Look for price column
    price_cols <- c("adjusted", "close", "price", "value")
    found_col <- NULL
    for (col in price_cols) {
      if (col %in% names(series)) {
        found_col <- col
        break
      }
    }
    if (!is.null(found_col)) {
      series_values <- as.numeric(series[[found_col]])
    } else {
      # Use first numeric column
      numeric_cols <- sapply(series, is.numeric)
      if (any(numeric_cols)) {
        series_values <- as.numeric(series[[which(numeric_cols)[1]]])
      } else {
        stop("No numeric columns found in data frame")
      }
    }
  } else {
    # Regular vector
    series_values <- as.numeric(series)
    original_length <- length(series_values)
    original_index <- seq_len(original_length)
  }

  # Ensure lengths match
  if (length(series_values) != length(original_index)) {
    stop(paste("Length mismatch: series_values has", length(series_values),
               "elements but original_index has", length(original_index), "elements"))
  }

  # Check if we have any non-NA values
  non_na_indices <- which(!is.na(series_values))
  if (length(non_na_indices) < 2) {
    stop("Need at least 2 non-NA values to calculate technical indicators")
  }

  # Fill NAs using a simple forward/backward fill approach
  filled_values <- series_values

  # Forward fill: carry last observation forward
  for (i in 2:length(filled_values)) {
    if (is.na(filled_values[i]) && !is.na(filled_values[i-1])) {
      filled_values[i] <- filled_values[i-1]
    }
  }

  # Backward fill: carry first observation backward
  for (i in (length(filled_values)-1):1) {
    if (is.na(filled_values[i]) && !is.na(filled_values[i+1])) {
      filled_values[i] <- filled_values[i+1]
    }
  }

  # If there are still NAs (shouldn't happen with proper data), use median of non-NA values
  if (any(is.na(filled_values))) {
    median_val <- median(series_values, na.rm = TRUE)
    filled_values[is.na(filled_values)] <- median_val
  }

  # Convert back to xts if original was xts
  if (xts::is.xts(series)) {
    filled_series <- xts::xts(filled_values, order.by = original_index)
    # Set column name based on what we selected
    if (ncol(series) > 1) {
      # For multi-column, use the name of the selected column
      price_cols <- c("adjusted", "close", "price", "value")
      found_col <- NULL
      for (col in price_cols) {
        if (col %in% colnames(series)) {
          found_col <- col
          break
        }
      }
      if (!is.null(found_col)) {
        colnames(filled_series) <- found_col
      } else {
        colnames(filled_series) <- colnames(series)[1]
      }
    } else {
      colnames(filled_series) <- colnames(series)
    }
  } else {
    filled_series <- filled_values
  }

  # Calculate SMAs on the filled series
  sma_signal <- TTR::SMA(filled_series, n = signal)
  sma_short <- TTR::SMA(filled_series, n = short)
  sma_long <- TTR::SMA(filled_series, n = long)
  sma_slow <- TTR::SMA(filled_series, n = slow)

  # Calculate the ratios (log of ratios)
  sma_0 <- log(filled_values / sma_signal)
  sma_1 <- log(sma_signal / sma_short)
  sma_2 <- log(sma_short / sma_long)
  sma_3 <- log(sma_long / sma_slow)

  sma_0_vel <- sma_0 - dplyr::lag(sma_0, 1)
  sma_1_vel <- sma_1 - dplyr::lag(sma_1, 1)
  sma_2_vel <- sma_2 - dplyr::lag(sma_2, 1)
  sma_3_vel <- sma_3 - dplyr::lag(sma_3, 1)

  # Create data frame with the results, ensuring same length as original
  df <- data.frame(
    sma_0 = as.numeric(sma_0),
    sma_1 = as.numeric(sma_1),
    sma_2 = as.numeric(sma_2),
    sma_3 = as.numeric(sma_3),
    sma_0_vel = as.numeric(sma_0_vel),
    sma_1_vel = as.numeric(sma_1_vel),
    sma_2_vel = as.numeric(sma_2_vel),
    sma_3_vel = as.numeric(sma_3_vel)
  )

  # Ensure the data frame has the same number of rows as original series
  if (nrow(df) != original_length) {
    # Pad with NAs if needed (shouldn't happen but safety check)
    if (nrow(df) < original_length) {
      missing_rows <- original_length - nrow(df)
      na_rows <- data.frame(
        sma_0 = rep(NA, missing_rows),
        sma_1 = rep(NA, missing_rows),
        sma_2 = rep(NA, missing_rows),
        sma_3 = rep(NA, missing_rows),
        sma_0_vel = rep(NA, missing_rows),
        sma_1_vel = rep(NA, missing_rows),
        sma_2_vel = rep(NA, missing_rows),
        sma_3_vel = rep(NA, missing_rows)
      )
      df <- rbind(na_rows, df)
    }
  }

  # Set row names to match the original series index exactly
  if (xts::is.xts(series)) {
    rownames(df) <- as.character(original_index)
  } else {
    rownames(df) <- as.character(original_index)
  }

  # Convert to xts if requested
  if (as.xts) {
    if (xts::is.xts(series)) {
      # Use the original index from the xts object
      df <- xts::xts(df, order.by = original_index)
    } else {
      # For non-xts input, try to convert rownames to dates
      df <- xts::xts(df, order.by = as.Date(rownames(df)))
    }
  }

  return(df)
}

build_features <- function(series) {
  series_garch <- garchvar(series)
  btc_ta <- tafeatures(series, as.xts = TRUE)
  aligned <- align(series, series_garch, btc_ta)
  exo <- withexovars(aligned)

  trend_returns <- na.omit(diff(log(exo[, 1])))
  trend_fit <- rcfit(trend_returns, regimes = 3)
  trend_regimes <- rcfitted(trend_fit) %>%
    as.data.frame() %>%
    dplyr::mutate(trend_regime = as.factor(as.character(regime))) %>%
    dplyr::select(trend_regime) %>%
    xts::as.xts(order.by = zoo::index(rcfitted(trend_fit)))

  vol_fit <- rcfit(na.omit(exo[, 2]), regimes = 3)
  vol_regimes <- rcfitted(vol_fit) %>%
    as.data.frame() %>%
    dplyr::mutate(vol_regime = as.factor(as.character(regime))) %>%
    dplyr::select(vol_regime) %>%
    xts::as.xts(order.by = zoo::index(rcfitted(vol_fit)))

  regimes <- merge(trend_regimes, vol_regimes, join = "inner")
  regimes

  final <- merge(exo, regimes, join = "left")
  final
}
