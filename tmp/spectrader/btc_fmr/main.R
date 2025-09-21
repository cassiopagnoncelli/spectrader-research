library("TTR")
library("forecast")

before <- function(...) {
  btc_fmr_ma5 <- lag(SMA(close("BSBTCUSDH1"), 5))
  btc_fmr_ma50 <- lag(SMA(close("BSBTCUSDH1"), 50))
  btc_fmr_signal <<- btc_fmr_ma5 > btc_fmr_ma50 & lag(btc_fmr_ma5) < lag(btc_fmr_ma50)

  macd <- MACD(close("BSBTCUSDH1"), 12, 80, 12)$macd
  fit <- auto.arima(macd)
  macd10 <<- forecast(fit, h = 10)$fitted
}

tick <- function() {
  if (Positions$count(OPEN) >= 3) { return() }
  if (btc_fmr_signal[I]) {
    pos_count <- Positions$count(OPEN)
    max_pos_size <- .internal$lifo$wallet$balance() / ask("BSBTCUSDH1")
    if (pos_count == 0) { pos_size <- floor(max_pos_size * 0.3) }
    if (pos_count == 1) { pos_size <- floor(max_pos_size * 0.45) }
    if (pos_count == 2) { pos_size <- floor(max_pos_size * 0.8) }
    buy(pos_size, ticker = "BSBTCUSDH1")
  }
}

position <- function() {
  trailing_stop(
    stop_loss = 0.15,
    sliding_lockin_level = 0.6,
    close_on_sideways = TRUE,
    sideways_bars_stop_loss = 50
  )
}
