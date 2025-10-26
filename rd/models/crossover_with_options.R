devtools::load_all()

options(scipen = 999)

fetl <- Fetl$new()

# Load and prepare data
dfm_raw <- fetl$send_query("
    SELECT
      c.symbol,
      q.close,
      differential_forward_mass(c.symbol, q.date, 30) AS dfm_0
    FROM quotes q
    JOIN companies c ON q.company_id = c.id
    WHERE q.date BETWEEN '2021-01-01' AND '2024-12-31' AND symbol = 'PLTR'
  ") %>%
  tibble

dfm_pre <- dfm_raw %>%
  dplyr::mutate(
    rsi_14 = TTR::RSI(close, n = 14),
    vol_0 = TTR::runSD(close, n = 10),
    vol_1 = lag(TTR::runSD(close, n = 10)),
    vol_2 = lag(TTR::runSD(close, n = 10), 2),
    sig_0 = TTR::SMA(close, n = 5),
    sig_1 = lag(TTR::SMA(close, n = 5)),
    sig_2 = lag(TTR::SMA(close, n = 5), 2),
    fast_0 = TTR::SMA(close, n = 20),
    fast_1 = lag(TTR::SMA(close, n = 20)),
    fast_2 = lag(TTR::SMA(close, n = 20), 2),
    slow_0 = TTR::SMA(close, n = 80),
    slow_1 = lag(TTR::SMA(close, n = 80)),
    slow_2 = lag(TTR::SMA(close, n = 80), 2)
  )

dfm <- dfm_pre %>%
  mutate(
    vol_vel = vol_0 - vol_1,
    vol_accel = vol_0 - 2 * vol_1 + vol_2,
    close_to_sig = close / sig_0,
    sig_fast_0 = sig_0 / fast_0,
    fast_slow_0 = fast_0 / slow_0,
    sig_vel = sig_0 - sig_1,
    sig_accel = sig_0 - 2 * sig_1 + sig_2,
    fast_vel = fast_0 - fast_1,
    fast_accel = fast_0 - 2 * fast_1 + fast_2,
    slow_vel = slow_0 - slow_1,
    slow_accel = slow_0 - 2 * slow_1 + slow_2
  ) %>%
  na.omit() %>%
  select(-c(
    symbol, close,
    vol_0, vol_1, vol_2,
    sig_0, sig_1, sig_2,
    fast_0, fast_1, fast_2,
    slow_0, slow_1, slow_2
  ))
dfm

