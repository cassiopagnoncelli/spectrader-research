#' Calculate the Classical Kelly Fraction for Optimal Position Sizing
#'
#' Computes the classical Kelly Criterion fraction (f*) for a series of returns, which
#' represents the optimal fraction of capital to risk on each trade to maximize
#' long-term growth rate. This implementation uses the traditional Kelly formula:
#' f* = (bp - q) / b, where b is the win/loss ratio, p is the probability of
#' winning, and q = 1 - p.
#'
#' @param rets Numeric vector of returns. Non-finite values (NA, NaN, Inf) are
#'   automatically removed before calculation.
#'
#' @return Numeric value between 0 and 1 representing the optimal Kelly fraction.
#'   Returns 0 if there are no winning trades, and 1 if there are no losing trades.
#'   The result is bounded between 0 and 1 to prevent negative or excessive leverage.
#'
#' @details
#' This function implements the classical Kelly Criterion, a formula used to determine
#' the optimal size of a series of bets to maximize logarithmic wealth. In trading,
#' it helps determine what fraction of capital should be risked on each trade.
#'
#' The classical Kelly formula calculates:
#' \itemize{
#'   \item \strong{p}: Probability of winning (proportion of positive returns)
#'   \item \strong{q}: Probability of losing (1 - p)
#'   \item \strong{b}: Win/loss ratio (average win / absolute average loss)
#'   \item \strong{f*}: Kelly fraction = (bp - q) / b
#' }
#'
#' Edge cases:
#' \itemize{
#'   \item If all trades are losses, returns 0 (no position)
#'   \item If all trades are wins, returns 1 (full position)
#'   \item Result is clamped to [0, 1] to prevent over-leveraging
#' }
#'
#' @references
#' Kelly, J. L. (1956). "A New Interpretation of Information Rate".
#' Bell System Technical Journal. 35 (4): 917-926.
#'
#' @examples
#' # Example with mixed returns
#' returns <- c(0.05, -0.02, 0.03, -0.01, 0.04, -0.03, 0.02)
#' kelly_fraction(returns)
#'
#' # Example with only winning trades
#' winning_returns <- c(0.05, 0.03, 0.04, 0.02)
#' kelly_fraction(winning_returns)
#'
#' # Example with losing trades
#' losing_returns <- c(-0.02, -0.01, -0.03)
#' kelly_fraction(losing_returns)
#'
#' @export
kelly_fraction <- function(rets) {
  rets <- rets[is.finite(rets)]
  p <- mean(rets > 0)
  q <- 1 - p
  win <- rets[rets > 0]
  loss <- rets[rets < 0]
  if (length(win) == 0) return(0)
  if (length(loss) == 0) return(1)
  b <- mean(win) / abs(mean(loss))
  f <- (b * p - q) / b
  max(0, min(1, f))
}

#' Calculate the Quantile-Based Kelly Fraction for Optimal Position Sizing
#'
#' Computes a quantile-based Kelly Criterion fraction (f_tau) that uses quantiles
#' of the win and loss distributions instead of means. This approach provides a
#' more robust position sizing strategy by considering the distributional tails
#' and downside risk through the quantile parameter tau. Unlike the classical Kelly
#' approach, this method is less sensitive to outliers and allows for risk-adjusted
#' position sizing through the tau parameter.
#'
#' @param returns Numeric vector of returns. The function automatically separates
#'   winning (positive) and losing (non-positive) returns for quantile calculation.
#'   Must be numeric and contain at least 2 observations.
#' @param tau Numeric value between 0 and 1 representing the quantile parameter.
#'   Default is 0.5 (median). Higher values (e.g., 0.75) focus on more optimistic
#'   scenarios, while lower values (e.g., 0.25) emphasize conservative tail risk.
#'
#' @return Numeric value between 0 and 1 representing the quantile-based Kelly fraction.
#'   Returns 0 if there are no winning trades or no losing trades (degenerate case).
#'   The result is clamped between 0 and 1 to prevent negative or excessive leverage.
#'
#' @details
#' Unlike the classical Kelly Criterion which uses mean returns, this quantile-based
#' approach calculates position size using distributional quantiles, providing greater
#' robustness to outliers and allowing explicit risk control through tau.
#'
#' The function performs the following steps:
#' \enumerate{
#'   \item Separates returns into wins (> 0) and losses (<= 0)
#'   \item Calculates win probability: p = n_wins / n_total
#'   \item Computes quantiles:
#'     \itemize{
#'       \item r_w: tau-quantile of winning returns
#'       \item r_l: (1-tau)-quantile of losing returns (complementary quantile)
#'     }
#'   \item Calculates: f_tau = (p * r_w - q * |r_l|) / r_w
#'   \item Clamps result to [0, 1]
#' }
#'
#' The quantile parameter tau controls the risk profile:
#' \itemize{
#'   \item \strong{tau = 0.5}: Uses median returns (balanced, robust approach)
#'   \item \strong{tau > 0.5}: More aggressive, emphasizes upside potential
#'   \item \strong{tau < 0.5}: More conservative, emphasizes downside protection
#' }
#'
#' This method is particularly useful when:
#' \itemize{
#'   \item Return distributions are non-normal, skewed, or fat-tailed
#'   \item You want to incorporate tail risk into position sizing decisions
#'   \item Mean-based estimates are unreliable due to extreme outliers
#'   \item A more conservative or risk-aware approach is desired
#'   \item You need to adjust position sizing based on distributional properties
#' }
#'
#' @section Comparison to Classical Kelly:
#' The classical Kelly Criterion (\code{\link{kelly_fraction}}) uses mean wins
#' and mean losses, which can be heavily influenced by outliers. The quantile-based
#' approach provides robustness by using quantiles, and allows practitioners to
#' explicitly adjust their risk profile through the tau parameter. When tau = 0.5,
#' this method uses medians, which are inherently more robust than means.
#'
#' @section Edge Cases:
#' \itemize{
#'   \item If all returns are positive (no losses), returns 0 (conservative default)
#'   \item If all returns are non-positive (no wins), returns 0 (no position)
#'   \item The function validates that returns is numeric with length > 1
#' }
#'
#' @examples
#' # Basic usage with mixed returns
#' returns <- c(0.05, -0.02, 0.03, -0.01, 0.04, -0.03, 0.02, -0.015)
#'
#' # Median-based (balanced approach)
#' kelly_quantile(returns, tau = 0.5)
#'
#' # Conservative approach (25th percentile)
#' kelly_quantile(returns, tau = 0.25)
#'
#' # Aggressive approach (75th percentile)
#' kelly_quantile(returns, tau = 0.75)
#'
#' # Compare different tau values
#' sapply(c(0.25, 0.5, 0.75), function(t) kelly_quantile(returns, tau = t))
#'
#' # Compare with classical Kelly
#' classical <- kelly_fraction(returns)
#' quantile_median <- kelly_quantile(returns, tau = 0.5)
#' cat(sprintf("Classical: %.3f, Quantile (median): %.3f\n", 
#'             classical, quantile_median))
#'
#' # Example with fat-tailed returns
#' set.seed(42)
#' fat_tailed <- rt(100, df = 3) * 0.02  # Student's t with 3 df
#' kelly_quantile(fat_tailed, tau = 0.5)
#'
#' @seealso
#' \code{\link{kelly_fraction}} for the classical Kelly Criterion implementation
#'
#' @export
kelly_quantile <- function(returns, tau = 0.5) {
  stopifnot(is.numeric(returns), length(returns) > 1)
  returns <- returns[is.finite(returns)]
  
  wins  <- returns[returns > 0]
  loss  <- returns[returns <= 0]
  p     <- length(wins) / length(returns)
  q     <- 1 - p
  
  if (length(wins) == 0) return(0)
  if (length(loss) == 0) return(1)
  
  r_w <- quantile(wins,  probs = tau, na.rm = TRUE)
  r_l <- quantile(loss,  probs = 1 - tau, na.rm = TRUE)
  
  f_tau <- (p * r_w - q * abs(r_l)) / r_w
  f_tau <- max(0, min(f_tau, 1))  # clamp 0–1
  
  return(f_tau)
}

#' Plot Classical Kelly Portfolio Growth Over Time
#'
#' Visualizes the cumulative portfolio growth using the classical Kelly Criterion for
#' position sizing. Creates a ggplot2 area chart showing how a portfolio would
#' grow when applying the classical Kelly fraction to a series of returns. Optionally
#' displays growth on a logarithmic scale.
#'
#' @param returns Numeric vector of returns for each trade.
#' @param kelly_p Numeric value between 0 and 1 representing the Kelly fraction
#'   to apply. If NULL (default), the optimal Kelly fraction is automatically
#'   calculated using \code{\link{kelly_fraction}}.
#' @param log.transform Logical. If TRUE, applies log10 transformation to the
#'   portfolio values for visualization. Useful when portfolio growth spans
#'   multiple orders of magnitude. Default is FALSE.
#'
#' @return A ggplot2 object representing the portfolio growth visualization.
#'   The plot is also printed to the current graphics device.
#'
#' @details
#' The function calculates portfolio growth using the classical Kelly approach as:
#' P_n = P_0 * prod(1 + r_i * f*)
#'
#' where P_n is the portfolio value at trade n, r_i is the return on trade i,
#' and f* is the classical Kelly fraction.
#'
#' The visualization includes:
#' \itemize{
#'   \item Green area chart showing portfolio evolution
#'   \item Solid line tracking the portfolio value
#'   \item Horizontal dotted line at 1.0 (starting capital)
#'   \item Title, subtitle with key statistics (number of trades, Kelly fraction)
#'   \item Minimal theme for clean presentation
#' }
#'
#' @section Dependencies:
#' Requires the following packages:
#' \itemize{
#'   \item ggplot2: For creating the visualization
#'   \item magrittr: For the pipe operator (%>%)
#' }
#'
#' @seealso \code{\link{kelly_fraction}} for calculating the optimal classical Kelly fraction
#'
#' @examples
#' \dontrun{
#' # Generate sample returns
#' set.seed(123)
#' returns <- rnorm(100, mean = 0.01, sd = 0.05)
#'
#' # Plot with automatically calculated Kelly fraction
#' plot_kelly_trades(returns)
#'
#' # Plot with custom Kelly fraction
#' plot_kelly_trades(returns, kelly_p = 0.25)
#'
#' # Plot with log transformation for large growth
#' large_returns <- rnorm(500, mean = 0.02, sd = 0.03)
#' plot_kelly_trades(large_returns, log.transform = TRUE)
#'
#' # Compare different Kelly fractions
#' returns <- c(0.05, -0.02, 0.03, -0.01, 0.04, -0.03, 0.02, 0.01)
#' plot_kelly_trades(returns, kelly_p = 0.5)
#' plot_kelly_trades(returns, kelly_p = 1.0)
#' }
#'
#' @export
plot_kelly_trades <- function(returns, kelly_p = NULL, log.transform = FALSE) {
  if (is.null(kelly_p)) {
    kelly_p <- kelly_fraction(returns)
  }

  portfolio <- cumprod(c(1, 1 + returns * kelly_p))
  if (log.transform) {
    portfolio <- log10(portfolio)
  }
  df <- data.frame(
    trade = seq_along(portfolio),
    portfolio = portfolio
  ) %>% na.omit()

  p <- ggplot2::ggplot(df, ggplot2::aes(x = trade, y = portfolio)) +
    ggplot2::geom_area(fill = "chartreuse4", alpha = 0.25) +
    ggplot2::geom_line(linewidth = 0.25, color = "chartreuse4") +
    ggplot2::geom_hline(yintercept = 1, linetype = "dotted", color = "gray50") +
    ggplot2::labs(
      title = "Kelly Portfolio Growth",
      subtitle = sprintf("%s, %d trades, f* = %.3f",
        ifelse(log.transform, "log10(P_n)", "P_n"),
        nrow(df),
        kelly_p
      ),
      x = "Trade Number",
      y = "Portfolio Value"
    ) +
    ggplot2::theme_minimal()
  print(p)
  p
}
