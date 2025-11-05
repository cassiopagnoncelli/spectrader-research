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
