library("TTR")
library("forecast")

before <- function(...) {
  aapl_ma5 <- lag(SMA(close("AAPL"), 5))
  aapl_ma50 <- lag(SMA(close("AAPL"), 50))
  aapl_signal <<- aapl_ma5 > aapl_ma50 & lag(aapl_ma5) < lag(aapl_ma50)

  macd <- MACD(close("AAPL"), 12, 80, 12)$macd
  fit <- auto.arima(macd)
  macd10 <<- forecast(fit, h = 10)$fitted
}

tick <- function() {
  if (Positions$count(OPEN) >= 3) {
    return()
  }
  if (aapl_signal[I]) {
    pos_count <- Positions$count(OPEN)
    max_pos_size <- .internal$lifo$wallet$balance() / ask("AAPL")
    if (pos_count == 0) {
      pos_size <- floor(max_pos_size * 0.3)
    }
    if (pos_count == 1) {
      pos_size <- floor(max_pos_size * 0.45)
    }
    if (pos_count == 2) {
      pos_size <- floor(max_pos_size * 0.8)
    }
    buy(pos_size, ticker = "AAPL")
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
