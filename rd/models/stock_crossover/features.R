# Feature Engineering for Stock Crossover Model
# This script builds the feature matrix (fwd) and metadata

prepare_fwd <- function(fetl, methods, days = 15, companies = 300, cache = TRUE) {
  # Start timing
  start_time <- Sys.time()

  # Cache management
  if (!is.null(cache)) {
    cached_data <- load_cache(cache)
    if (!is.null(cached_data))
      return(cached_data)

    cat(sprintf("Cache miss. Computing features...\n"))
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
  fwd_cols_sql <- paste(fwd_lines, collapse = ",\n")
  fwd_sql <- sprintf("
    SELECT
      c.symbol,
      q.date,
      q.close,
%s,
      fredu.vix
    FROM quotes q
    JOIN companies c ON q.company_id = c.id
    JOIN company_screener_ids(
      min_trade_date => '2021-01-01'::DATE,
      min_market_cap => 1.5e10,
      random_sample => TRUE,
      max_companies => %d
    ) c2 ON c.id = c2.id
    JOIN LATERAL (
      SELECT fu.value AS vix
      FROM fred_univariates fu
      WHERE fu.code = 'VIXCLS'
        AND fu.freq = 'D'
        AND fu.date <= q.date
      ORDER BY fu.date DESC
      LIMIT 1
    ) fredu ON TRUE
    WHERE
      q.date BETWEEN '2021-01-01' AND '2025-09-30'
  ", fwd_cols_sql, companies)

  # Load and prepare data
  fwd_raw <- fetl$send_query(fwd_sql) %>% tibble::tibble()
  fwd <- fwd_raw %>%
    dplyr::group_by(symbol) %>%
    # Preprocessing
    dplyr::mutate(
      r_t = log(close / lag(close)),

      # Raw indicators: technical, volatility, entropy
      rsi_0 = TTR::RSI(close, n = 14),
      rsi_1 = lag(rsi_0),
      rsi_2 = lag(rsi_0, 2),
      vol_0 = TTR::runSD(close, n = 10),
      vol_1 = lag(vol_0),
      vol_2 = lag(vol_0, 2),
      vol_9 = lag(vol_0, 9),
      vol_22 = lag(vol_0, 22),
      sig_0 = TTR::SMA(close, n = 5),
      sig_1 = lag(sig_0),
      sig_2 = lag(sig_0, 2),
      fast_0 = TTR::SMA(close, n = 20),
      fast_1 = lag(fast_0),
      fast_2 = lag(fast_0, 2),
      slow_0 = TTR::SMA(close, n = 80),
      slow_1 = lag(slow_0),
      slow_2 = lag(slow_0, 2),
      h_sig_0 = runH(log(close / lag(close)), 9),
      h_sig_1 = lag(h_sig_0),
      h_sig_2 = lag(h_sig_0, 2),
      h_slow_0 = runH(log(close / lag(close)), 60),
      h_slow_1 = lag(h_slow_0),
      h_slow_2 = lag(h_slow_0, 2),

      # Ratios
      close_to_sig_0 = log(close / sig_0),
      close_to_sig_1 = lag(close_to_sig_0),
      close_to_sig_2 = lag(close_to_sig_0, 2),
      close_to_fast_0 = log(close / fast_0),
      sig_fast_0 = log(sig_0 / fast_0),
      sig_fast_1 = lag(sig_fast_0),
      sig_fast_2 = lag(sig_fast_0, 2),
      fast_slow_0 = log(fast_0 / slow_0),
      fast_slow_1 = lag(fast_slow_0),
      fast_slow_2 = lag(fast_slow_0, 2),
      vol_ratio = pmax(1e-3, pmin(20,
        vol_9 / pmax(vol_22, quantile(vol_22, 0.05, na.rm = T))
      )),
      h_ratio_0 = pmax(1e-3, pmin(1e3,
        h_sig_0 / pmax(h_slow_0, quantile(h_slow_0, 0.05, na.rm = T))
      )),
      h_ratio_1 = lag(h_ratio_0),
      h_ratio_2 = lag(h_ratio_0, 2),

      # Motion indicators
      rsi_vel = rsi_0 - rsi_1,
      rsi_accel = rsi_0 - 2 * rsi_1 + rsi_2,
      vol_vel = vol_0 - vol_1,
      vol_accel = vol_0 - 2 * vol_1 + vol_2,
      sig_vel = sig_0 - sig_1,
      sig_accel = sig_0 - 2 * sig_1 + sig_2,
      fast_vel = fast_0 - fast_1,
      fast_accel = fast_0 - 2 * fast_1 + fast_2,
      slow_vel = slow_0 - slow_1,
      slow_accel = slow_0 - 2 * slow_1 + slow_2,
      h_sig_vel = h_sig_0 - h_sig_1,
      h_sig_accel = h_sig_0 - 2 * h_sig_1 + h_sig_2,
      h_slow_vel = h_slow_0 - h_slow_1,
      h_slow_accel = h_slow_0 - 2 * h_slow_1 + h_slow_2,

      # Ratios motion indicators
      close_to_sig_vel = close_to_sig_0 - close_to_sig_1,
      close_to_sig_accel = close_to_sig_0 - 2 * close_to_sig_1 + close_to_sig_2,
      sig_fast_vel = sig_fast_0 - sig_fast_1,
      sig_fast_accel = sig_fast_0 - 2 * sig_fast_1 + sig_fast_2,
      fast_slow_vel = fast_slow_0 - fast_slow_1,
      fast_slow_accel = fast_slow_0 - 2 * fast_slow_1 + fast_slow_2,
      h_ratio_vel = h_ratio_0 - h_ratio_1,
      h_ratio_accel = h_ratio_0 - 2 * h_ratio_1 + h_ratio_2
    ) %>%
    dplyr::ungroup() %>%
    na.omit()

  # Store symbol and date before removing them
  fwd_metadata <- fwd %>%
    dplyr::select(symbol, date)

  # Remove marker columns for training
  fwd <- fwd %>%
    dplyr::select(-c(
      symbol,
      date,
      close, r_t,
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
  if (!is.null(cache)) {
    cat(sprintf("Saving to cache: %s\n", ck$path))
    save_cache(ck, result)
    ck_sql <- cache_key(params = params, ext = "sql", fun = "prepare_fwd")
    writeLines(fwd_sql, ck_sql$path)
    cat(sprintf("✓ Computation completed in %.2f seconds (cached for future use)\n",
                as.numeric(elapsed)))
  } else {
    cat(sprintf("✓ Computation completed in %.2f seconds\n", as.numeric(elapsed)))
  }

  return(result)
}
