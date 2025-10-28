# Feature Engineering for Stock Crossover Model
# This script builds the feature matrix (fwd) and metadata

prepare_fwd <- function(fetl, methods, days = 15, companies = 300, cache = FALSE) {
  # Start timing
  start_time <- Sys.time()
  
  # Cache management
  if (cache) {
    # Create cache directory if it doesn't exist
    cache_dir <- "_cache_"
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }
    
    # Generate cache key based on parameters
    cache_params <- list(
      methods = sort(methods),  # Sort to ensure consistent ordering
      days = days,
      companies = companies
    )
    cache_key <- digest::digest(cache_params, algo = "md5")
    cache_file <- file.path(cache_dir, sprintf("prepare_fwd_%s.rds", cache_key))
    
    # Try to load from cache
    if (file.exists(cache_file)) {
      cat(sprintf("Loading cached data from: %s\n", cache_file))
      cached_data <- readRDS(cache_file)
      end_time <- Sys.time()
      elapsed <- difftime(end_time, start_time, units = "secs")
      cat(sprintf("✓ Cache hit - loaded in %.2f seconds\n", as.numeric(elapsed)))
      return(cached_data)
    } else {
      cat(sprintf("Cache miss. Computing features...\n"))
    }
  }
  
  # Ensure methods is an array
  if (!is.vector(methods)) {
    methods <- c(methods)
  }
  
  # Generate dynamic SQL lines for each method
  fwd_lines <- sapply(seq_along(methods), function(i) {
    method <- methods[i]
    if (i == 1) {
      sprintf("      fwd('%s', c.symbol, q.date, %d) AS y", method, days)
    } else {
      sprintf("      fwd('%s', c.symbol, q.date, %d) AS y_%d", method, days, i - 1)
    }
  })
  
  # Join lines with comma
  fwd_sql <- paste(fwd_lines, collapse = ",\n")
  
  # Load and prepare data
  fwd_raw <- fetl$send_query(sprintf("
    SELECT
      c.symbol,
      q.date,
      q.close,
%s
    FROM quotes q
    JOIN companies c ON q.company_id = c.id
    JOIN company_screener_ids(
      min_trade_date => '2021-01-01'::DATE,
      min_market_cap => 1.5e10,
      random_sample => TRUE,
      max_companies => %d
    ) c2 ON c.id = c2.id
    WHERE
      q.date BETWEEN '2021-01-01' AND '2025-09-30'
  ", fwd_sql, companies)) %>%
    tibble

  fwd <- fwd_raw %>%
    group_by(symbol) %>%
    # Preprocessing
    dplyr::mutate(
      rsi_0 = TTR::RSI(close, n = 14),
      rsi_1 = lag(TTR::RSI(close, n = 14)),
      rsi_2 = lag(TTR::RSI(close, n = 14), 2),
      vol_0 = TTR::runSD(close, n = 10),
      vol_1 = lag(TTR::runSD(close, n = 10)),
      vol_2 = lag(TTR::runSD(close, n = 10), 2),
      vol_9 = lag(TTR::runSD(close, n = 10), 9),
      vol_22 = lag(TTR::runSD(close, n = 10), 22),
      sig_0 = TTR::SMA(close, n = 5),
      sig_1 = lag(TTR::SMA(close, n = 5)),
      sig_2 = lag(TTR::SMA(close, n = 5), 2),
      fast_0 = TTR::SMA(close, n = 20),
      fast_1 = lag(TTR::SMA(close, n = 20)),
      fast_2 = lag(TTR::SMA(close, n = 20), 2),
      slow_0 = TTR::SMA(close, n = 80),
      slow_1 = lag(TTR::SMA(close, n = 80)),
      slow_2 = lag(TTR::SMA(close, n = 80), 2)
    ) %>%
    # Calculated indicators
    mutate(
      rsi_vel = rsi_0 - rsi_1,
      rsi_accel = rsi_0 - 2 * rsi_1 + rsi_2,
      vol_vel = vol_0 - vol_1,
      vol_accel = vol_0 - 2 * vol_1 + vol_2,
      close_to_sig = close / sig_0,
      close_to_fast = close / fast_0,
      sig_fast_0 = sig_0 / fast_0,
      fast_slow_0 = fast_0 / slow_0,
      sig_vel = sig_0 - sig_1,
      sig_accel = sig_0 - 2 * sig_1 + sig_2,
      fast_vel = fast_0 - fast_1,
      fast_accel = fast_0 - 2 * fast_1 + fast_2,
      slow_vel = slow_0 - slow_1,
      slow_accel = slow_0 - 2 * slow_1 + slow_2
    ) %>%
    ungroup() %>%
    na.omit()

  # Store symbol and date before removing them
  fwd_metadata <- fwd %>%
    select(symbol, date)

  # Remove marker columns for training
  fwd <- fwd %>%
    select(-c(
      symbol,
      date,
      close,
      sig_0, sig_1, sig_2,
      fast_0, fast_1, fast_2,
      slow_0, slow_1, slow_2
    ))

  # Return both the feature matrix and metadata
  result <- list(
    fwd = fwd,
    fwd_metadata = fwd_metadata
  )
  
  # Calculate elapsed time
  end_time <- Sys.time()
  elapsed <- difftime(end_time, start_time, units = "secs")
  
  # Save to cache if enabled
  if (cache) {
    cat(sprintf("Saving to cache: %s\n", cache_file))
    saveRDS(result, cache_file)
    cat(sprintf("✓ Computation completed in %.2f seconds (cached for future use)\n", as.numeric(elapsed)))
  } else {
    cat(sprintf("✓ Computation completed in %.2f seconds\n", as.numeric(elapsed)))
  }
  
  return(result)
}
