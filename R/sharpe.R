# ==========================================================
#  Quantitative Performance Metrics
#  Sharpe Ratio, Probabilistic Sharpe Ratio (PSR),
#  and Deflated Sharpe Ratio (DSR)
#  Assumes `rf_annual` is annualized risk-free rate.
# ==========================================================

#' Convert Annual Risk-Free Rate to Periodic Rate
#'
#' Converts an annualized risk-free rate into its equivalent
#' periodic rate, given the number of periods per year.
#'
#' @param rf_annual Annualized risk-free rate (e.g. 0.045 for 4.5%)
#' @param scale Number of periods per year (252 = daily, 52 = weekly, 12 = monthly)
#'
#' @return Period-equivalent risk-free rate
#'
#' @examples
#' rf_period(0.045, 252)
#'
#' @export
rf_period <- function(rf_annual, scale) {
  (1 + rf_annual)^(1 / scale) - 1
}

#' Annualized Sharpe Ratio
#'
#' Computes the annualized Sharpe ratio:
#' \deqn{SR = (E[R - R_f]) / sd(R - R_f) * \sqrt{scale}}
#'
#' Measures risk-adjusted performance as the ratio of mean excess return
#' to its standard deviation, scaled by the square root of frequency.
#'
#' @param returns Numeric vector of periodic returns.
#' @param rf_annual Annualized risk-free rate (default = 0.045).
#' @param scale Observations per year (default = 252 for daily data).
#'
#' @return Numeric Sharpe ratio.
#'
#' @examples
#' set.seed(42)
#' r <- rnorm(252, mean = 0.001, sd = 0.02)
#' sharpe_ratio(r, rf_annual = 0.045)
#'
#' @export
sharpe_ratio <- function(returns, rf_annual = 0.045, scale = 252, na.rm = TRUE) {
  rf <- rf_period(rf_annual, scale)
  excess <- returns - rf
  mean(excess, na.rm = na.rm) / sd(excess, na.rm = na.rm) * sqrt(scale)
}

#' Probabilistic Sharpe Ratio (PSR)
#'
#' Estimates the probability that the true Sharpe ratio exceeds
#' a chosen benchmark \eqn{SR^*}, adjusting for skewness (\eqn{\gamma_3})
#' and kurtosis (\eqn{\gamma_4}) of returns.
#'
#' Formula:
#' \deqn{
#' PSR(SR^*) = \Phi \left(
#'   \frac{(SR̂ - SR^*) \sqrt{n - 1}}
#'        {\sqrt{1 - \gamma_3 SR̂ + ((\gamma_4 - 1)/4) SR̂^2}}
#' \right)
#' }
#'
#' @param returns Numeric vector of periodic returns.
#' @param sr_benchmark Sharpe ratio benchmark to test against (default = 0).
#' @param rf_annual Annualized risk-free rate (default = 0.045).
#' @param scale Observations per year (default = 252).
#'
#' @return A list with components:
#' \itemize{
#'   \item \code{SR}  - Observed annualized Sharpe ratio.
#'   \item \code{PSR} - Probabilistic Sharpe ratio value.
#'   \item \code{z}   - Corresponding z-score.
#' }
#'
#' @examples
#' set.seed(42)
#' r <- rnorm(252, mean = 0.001, sd = 0.02)
#' psr(r, sr_benchmark = 0, rf_annual = 0.045)
#'
#' @export
#
# Tl;dr:
#
# PSR := P(true sharpe > sr_benchmark)
#
psr <- function(returns, sr_benchmark = 0, rf_annual = 0.045, scale = 252) {
  rf <- rf_period(rf_annual, scale)
  r <- returns - rf
  n <- length(r)
  sr_hat <- mean(r) / sd(r) * sqrt(scale)
  g3 <- moments::skewness(r)
  g4 <- moments::kurtosis(r)
  
  z <- (sr_hat - sr_benchmark) * sqrt(n - 1) /
       sqrt(1 - g3 * sr_hat + ((g4 - 1) / 4) * sr_hat^2)
  list(SR = sr_hat, PSR = pnorm(z), z = z)
}

#' Deflated Sharpe Ratio (DSR)
#'
#' Adjusts the Probabilistic Sharpe Ratio (PSR) for multiple testing (M strategies)
#' and finite sample bias, accounting for skewness and kurtosis.
#' Indicates the probability that the observed Sharpe ratio exceeds
#' the expected maximum Sharpe from \eqn{M} unskilled strategies.
#'
#' Formula:
#' \deqn{
#' DSR = \Phi \left(
#'   \frac{SR̂ - SR₀}{\sqrt{\sigmâ_{SR}^2 + \sigma_{SR₀}^2}}
#' \right)
#' }
#'
#' where:
#' \deqn{
#' \sigmâ_{SR}^2 = \frac{1 - \gamma_3 SR̂ + ((\gamma_4 - 1)/4) SR̂^2}{n - 1}
#' }
#' \deqn{
#' SR₀ = \sqrt{2 \ln M} - \frac{\ln \ln M + \ln 4\pi}{2 \sqrt{2 \ln M}}
#' }
#'
#' @param returns Numeric vector of periodic returns.
#' @param M Number of independent strategy trials.
#' @param rf_annual Annualized risk-free rate (default = 0.045).
#' @param scale Observations per year (default = 252).
#'
#' @return A list with components:
#' \itemize{
#'   \item \code{SR}  - Observed annualized Sharpe ratio.
#'   \item \code{DSR} - Deflated Sharpe ratio value.
#'   \item \code{z}   - Corresponding z-score.
#' }
#'
#' @examples
#' set.seed(42)
#' r <- rnorm(252, mean = 0.001, sd = 0.02)
#' dsr(r, M = 100, rf_annual = 0.045)
#'
#' @export
#
# Tl;dr:
#
# DSR := P(true sharpe > expected max sharpe of M trials)
#
dsr <- function(returns, M, rf_annual = 0.045, scale = 252) {
  rf <- rf_period(rf_annual, scale)
  r <- returns - rf
  n <- length(r)
  sr_hat <- mean(r) / sd(r) * sqrt(scale)
  g3 <- moments::skewness(r)
  g4 <- moments::kurtosis(r)
  
  sr_sigma <- sqrt((1 - g3 * sr_hat + ((g4 - 1) / 4) * sr_hat^2) / (n - 1))
  sr_0 <- sqrt(2 * log(M)) - (log(log(M)) + log(4 * pi)) / (2 * sqrt(2 * log(M)))
  sr_0_sigma <- 1 / sqrt(2 * log(M))
  
  z <- (sr_hat - sr_0) / sqrt(sr_sigma^2 + sr_0_sigma^2)
  list(SR = sr_hat, DSR = pnorm(z), z = z)
}
