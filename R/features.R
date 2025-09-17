calcreturns <- function(series, log = FALSE) {
  if (log) {
    return(diff(log(series)) / lag(log(series), 1))
  }
  diff(series) / lag(series, 1)
}

# Helper function to convert different input types to xts
convert_to_xts <- function(series, series_name = "series") {
  if (xts::is.xts(series)) {
    # Already an xts object, return as-is
    return(series)
  } else if (is.data.frame(series) || tibble::is_tibble(series)) {
    # Handle data.frame or tibble

    # Try to identify date column
    date_col <- NULL
    for (col in names(series)) {
      if (inherits(series[[col]], c("Date", "POSIXct", "POSIXt")) ||
          tolower(col) %in% c("date", "time", "datetime", "timestamp")) {
        date_col <- col
        break
      }
    }

    if (is.null(date_col)) {
      stop(paste("No date column found in", series_name, ". Expected column named 'date', 'time', 'datetime', or 'timestamp', or a column with Date/POSIXct class."))
    }

    # Extract dates
    dates <- series[[date_col]]

    # Find value columns (exclude date column)
    value_cols <- setdiff(names(series), date_col)

    if (length(value_cols) == 0) {
      stop(paste("No value columns found in", series_name, "after excluding date column."))
    }

    # Try common financial data column names first
    preferred_cols <- c("adjusted", "close", "value", "price")
    value_col <- NULL

    for (pref_col in preferred_cols) {
      if (pref_col %in% value_cols) {
        value_col <- pref_col
        break
      }
    }

    # If no preferred column found, use the first numeric column
    if (is.null(value_col)) {
      numeric_cols <- value_cols[sapply(value_cols, function(x) is.numeric(series[[x]]))]
      if (length(numeric_cols) > 0) {
        value_col <- numeric_cols[1]
      } else {
        stop(paste("No numeric value columns found in", series_name))
      }
    }

    # Create xts object
    xts_obj <- xts::xts(series[[value_col]], order.by = as.Date(dates))
    colnames(xts_obj) <- value_col

    return(xts_obj)

  } else {
    stop(paste("Input", series_name, "must be a data.frame, tibble, or xts object"))
  }
}

withexovars <- function(..., indexed = FALSE) {
  # Fetch common exogenous variables for financial time series modeling.
  vix <- get_ticker("VIX")[, "close"]       # xts object
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

  # Convert user-provided series to xts objects
  user_series_raw <- list(...)
  user_series <- list()
  user_series_names <- c()

  if (length(user_series_raw) > 0) {
    # Get the original variable names from the calling environment
    call_names <- as.character(substitute(list(...)))[-1]

    for (i in seq_along(user_series_raw)) {
      series_name <- if (i <= length(call_names)) call_names[i] else paste0("user_series_", i)
      converted_series <- convert_to_xts(user_series_raw[[i]], series_name)

      # Set a meaningful column name based on the original variable name and detected column
      original_col_name <- colnames(converted_series)[1]
      new_col_name <- paste(series_name, original_col_name, sep = "_")
      colnames(converted_series) <- new_col_name

      user_series[[i]] <- converted_series
      user_series_names <- c(user_series_names, series_name)
    }
  }

  # Create named lists for alignment
  all_series <- c(user_series,
                  list(vix = vix,
                       sp500 = sp500,
                       dxy = dxy,
                       cryptocap = cryptocap,
                       cryptogrowth = cryptogrowth,
                       bonds = bonds,
                       jobs = jobs,
                       oil = oil,
                       goldvol = goldvol))

  # Use align with names = FALSE to avoid automatic name generation
  exo <- do.call(align, c(all_series, list(names = FALSE)))

  # Create the final data frame with proper column names
  exo_df <- data.frame(
    date = as.Date(zoo::index(exo)),
    as.data.frame(xts::coredata(exo))
  )

  # Set proper column names for the data frame
  expected_col_names <- c("date")
  if (length(user_series) > 0) {
    for (i in seq_along(user_series)) {
      series_name <- if (i <= length(user_series_names)) user_series_names[i] else paste0("user_series_", i)
      original_col_name <- colnames(user_series[[i]])[1]
      # Remove the series_name prefix we added earlier since we're setting the final name
      clean_col_name <- sub(paste0("^", series_name, "_"), "", original_col_name)
      expected_col_names <- c(expected_col_names, paste(series_name, clean_col_name, sep = "_"))
    }
  }
  expected_col_names <- c(expected_col_names, "vix", "sp500_returns", "dxy", "crypto_cap_log",
                         "crypto_returns", "bonds_returns", "jobs_returns", "oil_returns", "gold_vol")

  # Ensure we have the right number of column names
  if (length(expected_col_names) == ncol(exo_df)) {
    colnames(exo_df) <- expected_col_names
  }

  # Handle indexed parameter
  if (indexed) {
    # Set rownames to dates and remove the date column
    rownames(exo_df) <- as.character(exo_df$date)
    exo_df <- exo_df[, !names(exo_df) %in% "date", drop = FALSE]
  }

  return(exo_df)
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
    series_returns_xts <- diff(series) / lag(series, 1)
    series_returns_xts <- series_returns_xts[!is.na(series_returns_xts)]

  } else {
    stop("Input must be a data frame with date/price columns or an xts object")
  }

  # Remove any remaining NA values
  series_returns_xts <- series_returns_xts[!is.na(series_returns_xts)]

  # Check if we have enough data
  if (length(series_returns_xts) < 10) {
    stop("Insufficient data for GARCH modeling (need at least 10 observations)")
  }

  # Specify GARCH(1,1) model
  garch_spec <- rugarch::ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
    distribution.model = "norm"
  )

  # Fit GARCH model
  garch_fit <- rugarch::ugarchfit(spec = garch_spec, data = series_returns_xts)

  # Return fitted values
  fitted_values <- rugarch::fitted(garch_fit)
  colnames(fitted_values) <- "volatility"
  return(fitted_values)
}

tafeatures <- function(series, slow = 200, long = 80, short = 20, signal = 8) {
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
    original_length <- length(series)
    series_values <- as.numeric(series)
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
    original_length <- length(series)
    original_index <- seq_len(original_length)
    series_values <- as.numeric(series)
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
    colnames(filled_series) <- colnames(series)
  } else {
    filled_series <- filled_values
  }

  # Calculate SMAs on the filled series
  sma_signal <- TTR::SMA(filled_series, n = signal)
  sma_short <- TTR::SMA(filled_series, n = short)
  sma_long <- TTR::SMA(filled_series, n = long)
  sma_slow <- TTR::SMA(filled_series, n = slow)

  # Calculate the ratios (log of ratios)
  sma_1 <- log(sma_signal / sma_short)
  sma_2 <- log(sma_short / sma_long)
  sma_3 <- log(sma_long / sma_slow)

  # Create data frame with the results, ensuring same length as original
  df <- data.frame(
    sma_1 = as.numeric(sma_1),
    sma_2 = as.numeric(sma_2),
    sma_3 = as.numeric(sma_3)
  )

  # Ensure the data frame has the same number of rows as original series
  if (nrow(df) != original_length) {
    # Pad with NAs if needed (shouldn't happen but safety check)
    if (nrow(df) < original_length) {
      missing_rows <- original_length - nrow(df)
      na_rows <- data.frame(
        sma_1 = rep(NA, missing_rows),
        sma_2 = rep(NA, missing_rows),
        sma_3 = rep(NA, missing_rows)
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

  return(df)
}
