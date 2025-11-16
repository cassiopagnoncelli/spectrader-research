#' Optimal Exit Boundary via First-Passage Time
#'
#' Computes the optimal stopping boundary for expected value maximization
#' using first-passage time theory for a geometric Brownian motion.
#'
#' @param mu Drift parameter of the underlying process
#' @param sigma Volatility parameter (standard deviation)
#' @param r Discount rate
#' @param K Strike or barrier level
#' @param t Time parameter in (0, 1) for finite horizon adjustment
#' @param side Position type: "long" or "short"
#'
#' @return Numeric value of the optimal exit boundary
#' @export
exit_fpt_boundary <- function(mu, sigma, r, K, t, side = c("long", "short")) {
  side <- match.arg(side)
  if (abs(t) >= 1)
    stop("t must be in (0, 1)")

  # small time penalty for finite horizon
  lambda <- 0.5 * (1 - t) * r

  # common terms
  root_term <- sqrt((mu / sigma^2 - 0.5)^2 + 2 * (r + lambda) / sigma^2)

  if (side == "long") {
    beta <- 0.5 - mu / sigma^2 + root_term
    K * beta / (beta - 1)
  } else {
    beta <- 0.5 - mu / sigma^2 - root_term  # negative root
    K * beta / (beta - 1)
  }
}
