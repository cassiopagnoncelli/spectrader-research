library("devtools")

devtools::load_all()

library("tidyquant")
library("dplyr")
library("lubridate")
library("xts")
library("zoo")
library("rugarch")
library("ggplot2")
library("TTR")

btc <- get_ticker("CBBTCUSD")
btc_garch <- garchvar(btc)
btc_ta <- tafeatures(btc, as.xts = TRUE)

aligned <- align(btc, btc_garch, btc_ta)

exo <- withexovars(aligned)

trend_returns <- na.omit(diff(log(exo[, 1])))
trend_fit <- rcfit(trend_returns, regimes = 3)
trend_regimes <- rcfitted(trend_fit) %>%
  as.data.frame() %>%
  mutate(trend_regime = as.factor(as.character(regime))) %>%
  dplyr::select(trend_regime) %>%
  as.xts(order.by = index(rcfitted(trend_fit)))

vol_fit <- rcfit(na.omit(exo[, 2]), regimes = 3)
vol_regimes <- rcfitted(vol_fit) %>%
  as.data.frame() %>%
  mutate(vol_regime = as.factor(as.character(regime))) %>%
  dplyr::select(vol_regime) %>%
  as.xts(order.by = index(rcfitted(vol_fit)))

regimes <- merge(trend_regimes, vol_regimes, join = "inner")
regimes
