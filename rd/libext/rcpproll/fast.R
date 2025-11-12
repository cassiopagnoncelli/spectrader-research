# =========================================================
# Benchmark: dplyr vs data.table vs RcppRoll (1M rows)
# =========================================================
library(dplyr)
library(data.table)
library(TTR)
library(RcppRoll)
library(furrr)
library(bench)

set.seed(42)

# ---------------------------------------------------------
# Synthetic data: 1000 symbols × 1000 closes = 1e6 rows
# ---------------------------------------------------------
symbols <- paste0("SYM", sprintf("%04d", 1:1000))
fwd_raw <- data.frame(
  symbol = rep(symbols, each = 1000),
  close = rnorm(1e6, mean = 0.01, sd = 0.015)
)
fwd_raw$close <- exp(cumsum(fwd_raw$close) / 10000)

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
      slow_0 = TTR::SMA(close, n = 80)
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
    slow_0 = SMA(close, n = 80)
  ), by = symbol]
  DT[]
}

# ---------------------------------------------------------
# 3️⃣ Ultra-optimized: data.table + RcppRoll
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
# 4️⃣ Parallelized: furrr + TTR (splitting by symbol)
# ---------------------------------------------------------
run_parallel <- function(df) {
  plan(multisession, workers = parallel::detectCores() - 1)
  fwd_list <- split(df, df$symbol)
  res_list <- furrr::future_map(fwd_list, function(d) {
    d$r_t <- log(d$close / dplyr::lag(d$close))
    d$vol_0 <- TTR::runSD(d$close, n = 10)
    d$sig_0 <- TTR::SMA(d$close, n = 5)
    d$fast_0 <- TTR::SMA(d$close, n = 20)
    d$slow_0 <- TTR::SMA(d$close, n = 80)
    d
  }, .progress = TRUE)
  data.table::rbindlist(res_list)
}

# ---------------------------------------------------------
# Optional: set number of threads for data.table / OpenMP
# ---------------------------------------------------------
data.table::setDTthreads(parallel::detectCores())

# ---------------------------------------------------------
# Run the benchmark
# ---------------------------------------------------------
bench::mark(
  # dplyr = run_dplyr(fwd_raw),
  # datatable = run_dt(fwd_raw),
  rcpproll = run_rcpproll(fwd_raw),
  # parallel = run_parallel(fwd_raw),
  iterations = 3,
  check = FALSE,
  memory = TRUE,
  time_unit = "s"
)
