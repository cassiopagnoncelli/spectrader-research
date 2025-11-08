#' @title American Call Option Pricing
#' @description Calculates American call option values using RQuantLib
#' @param S_0 Underlying asset price
#' @param S_t Strike price
#' @param t Time to maturity (days)
#' @param vol Implied volatility
#' @return Vector of option values
american_optprice <- function(S_0, S_t, t, vol) {
  mapply(function(s0, st, tm, v) {
    RQuantLib::AmericanOption(
      "call",
      underlying = s0,
      strike = st,
      dividendYield = 0,

      # Annual risk-free rate
      riskFreeRate = 0.0409,

      # Days in year
      maturity = tm / 365,

      # Implied volatility
      volatility = v
    )$value
  }, S_0, S_t, t, vol)
}

#' @title American Option Returns Calculator
#' @description Calculates option entry, exit prices and returns for given data
#' @param data Data frame with time (t) and returns (R) columns
#' @param K Strike price
#' @param tm Time to maturity (days)
#' @return Data frame with added option columns (opt_entry, opt_exit, opt_R)
american_optprice_returns <- function(data, K, tm, vol_0 = NA, vol_t = NA) {
  vol_0 <- ifelse(is.na(vol_0), runif(1, .6, 1.3), vol_0)
  vol_t <- ifelse(is.na(vol_t), vol_0 * runif(1, .8, 1.2), vol_t)
  data %>%
    dplyr::mutate(
      opt_entry = american_optprice(1, K, tm, vol_0),
      opt_exit = ifelse(t < tm, american_optprice(1 + R, K, tm - t - 1, vol_t), 0),
      opt_R = (opt_exit - opt_entry) / opt_entry
    )
}
