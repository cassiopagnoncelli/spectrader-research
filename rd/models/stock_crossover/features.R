# Feature Engineering for Stock Crossover Model
# This script builds the feature matrix (dfm) and metadata

prepare_dfm <- function(fetl) {
  # Load and prepare data
  dfm_raw <- fetl$send_query("
    SELECT
      c.symbol,
      q.date,
      q.close,
      differential_forward_mass(c.symbol, q.date, 15) AS dfm_0
    FROM quotes q
    JOIN companies c ON q.company_id = c.id
    JOIN company_screener_ids(
      min_trade_date => '2021-01-01'::DATE,
      min_market_cap => 2.5e10,
      max_market_cap => 9.0e10,
      random_sample => TRUE,
      max_companies => 100
    ) c2 ON c.id = c2.id
    WHERE
      q.date BETWEEN '2021-01-01' AND '2025-09-30'
  ") %>%
    tibble

  dfm <- dfm_raw %>%
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
  dfm_metadata <- dfm %>%
    select(symbol, date)

  # Remove marker columns for training
  dfm <- dfm %>%
    select(-c(
      symbol,
      date,
      close,
      sig_0, sig_1, sig_2,
      fast_0, fast_1, fast_2,
      slow_0, slow_1, slow_2
    ))

  # Return both the feature matrix and metadata
  list(
    dfm = dfm,
    dfm_metadata = dfm_metadata
  )
}
