library("devtools")

load_all()

library("tidyquant")
library("dplyr")
library("glmnet")
library("tibble")
library("MASS")

btc <- get_ticker("BSBTCUSDH1")
btc_series <- btc[, "adjusted"]
X <- build_features(btc_series)[, c("trend_regime", "vol_regime")]
X$trend_regime_past <- dplyr::lag(X$trend_regime, 1)
X$vol_regime_past <- dplyr::lag(X$vol_regime, 1)

# Predictions
joint <- tibble(tibble(timestamp = zoo::index(X)), as_tibble(X))
signals <- joint[complete.cases(joint), ] %>%
  mutate(
    trend_regime = as.integer(trend_regime),
    trend_regime_past = as.integer(trend_regime_past),
    vol_regime = as.integer(vol_regime),
    vol_regime_past = as.integer(vol_regime_past)
  ) %>%
  mutate(
    signal = trend_regime_past != 1 &
             trend_regime == 1
  ) %>%
  filter(signal == TRUE) %>%
  dplyr::select(timestamp)

signals$timestamp

# Entry profiler
entry_profiler(btc_series, signals$timestamp, lookback = 15, lookahead = 50)
