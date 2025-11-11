# ---------------------------------------------------------
# Benchmark: dplyr vs data.table vs data.table + RcppRoll
# ---------------------------------------------------------
library(dplyr)
library(TTR)
library(data.table)
library(RcppRoll)
library(bench)

set.seed(42)

# Synthetic data: 100 symbols × 1000 closes
symbols <- paste0("SYM", sprintf("%03d", 1:100))
fwd_raw <- data.frame(
  symbol = rep(symbols, each = 1000),
  close = rnorm(1000000, mean = 0.01, sd = 0.015)
)
# Add slight trend to avoid weird ratios
fwd_raw$close <- exp(cumsum(fwd_raw$close) / 100)

# ---------------------------------------------------------
# 1️⃣ Baseline: dplyr + TTR
# ---------------------------------------------------------
run_dplyr <- function(df) {
  df %>%
    dplyr::group_by(symbol) %>%
    dplyr::mutate(
      r_t = log(close / lag(close)),
      rsi_0 = TTR::RSI(close, n = 14),
      vol_0 = TTR::runSD(close, n = 10),
      sig_0 = TTR::SMA(close, n = 5),
      fast_0 = TTR::SMA(close, n = 20),
      slow_0 = TTR::SMA(close, n = 80),
      h_sig_0 = runH(log(close / lag(close)), 9),
      h_slow_0 = runH(log(close / lag(close)), 60)
    ) %>%
    dplyr::ungroup()
}

# ---------------------------------------------------------
# 2️⃣ Optimized: data.table + TTR
# ---------------------------------------------------------
run_dt <- function(df) {
  DT <- as.data.table(df)
  DT[, `:=`(
    r_t = log(close / shift(close)),
    rsi_0 = RSI(close, n = 14),
    vol_0 = runSD(close, n = 10),
    sig_0 = SMA(close, n = 5),
    fast_0 = SMA(close, n = 20),
    slow_0 = SMA(close, n = 80),
    h_sig_0 = runH(log(close / shift(close)), 9),
    h_slow_0 = runH(log(close / shift(close)), 60)
  ), by = symbol]
  DT[]
}

# ---------------------------------------------------------
# 3️⃣ Ultra-Optimized: data.table + RcppRoll
# ---------------------------------------------------------
run_rcpproll <- function(df) {
  DT <- as.data.table(df)
  DT[, `:=`(
    r_t = log(close / shift(close)),
    vol_0 = RcppRoll::roll_sd(close, 10, fill = NA, align = "right"),
    sig_0 = RcppRoll::roll_mean(close, 5, fill = NA, align = "right"),
    fast_0 = RcppRoll::roll_mean(close, 20, fill = NA, align = "right"),
    slow_0 = RcppRoll::roll_mean(close, 80, fill = NA, align = "right")
  ), by = symbol]
  DT[]
}

# ---------------------------------------------------------
# Run benchmark
# ---------------------------------------------------------
bench::mark(
  # dplyr = run_dplyr(fwd_raw),
  # datatable = run_dt(fwd_raw),
  rcpproll = run_rcpproll(fwd_raw),
  iterations = 5,
  check = FALSE
)
