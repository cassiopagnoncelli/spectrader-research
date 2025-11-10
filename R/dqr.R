#' @importFrom magrittr %>%
NULL

#' Train quantile regression models for exit analysis
#'
#' Fits multiple quantile regression models using position cohort data.
#'
#' @param signals Signal data for position cohort generation
#' @param taus Numeric vector of quantile values (e.g., c(0.92, 0.82, 0.32))
#' @param formulas List of formulas for each quantile (same length as taus)
#' @param max_position_days Maximum days after position entry (default: 60)
#' @return Named list of fitted models with format q<quantile> (e.g., q92, q82)
#' @examples
#' \dontrun{
#' train_dqr(my_signals, c(0.92, 0.82), list(S ~ S_1 + t, S ~ S_1 + t + vix))
#' }
#' @export
#
# Formulas
#
# form_full <- S ~ t + r +
#   sd_short + sd_long + sd_ratio + h_short + h_long + h_ratio +
#   sd_short_1 + sd_long_1 + sd_ratio_1 + h_short_1 + h_long_1 + h_ratio_1 +
#   vix + vol_vix +
#   cr_short + cr_long
#
# q92 <- S ~ S_1 + t + h_short + h_ratio + cr_long
# q82 <- S ~ S_1 + t + h_short + h_ratio + cr_long
# q32 <- S ~ S_1 + t + h_long + cr_long + vix
#
train_dqr <- function(signals, taus, formulas, max_position_days = 60) {
  if (length(taus) != length(formulas))
    stop("taus and formulas must have the same length")
  
  # Positions cohorts enriched for decaying quantile regression features
  posl <- position_cohort(
    signals,
    before_days = 30,
    after_days = max_position_days,
    fun = fe_dqr
  )

  # Amalgamate all positions into a single data frame
  data <- purrr::map_dfr(seq_along(posl), \(i) {
    posl[[i]] %>%
      dplyr::filter(t > 0) %>%
      dplyr::mutate(position_id = i, t_norm = t / max(t)) %>%
      na.omit()
  })
  
  models <- purrr::map2(
    formulas, taus,
    \(formula, tau) quantreg::rq(formula, tau = tau, data = data)
  )
  names(models) <- paste0("q", sprintf("%.0f", taus * 100))
  models
}

#' Pinball loss for quantile regression
#'
#' Computes the asymmetric pinball loss function for evaluating quantile predictions.
#'
#' @param actual Numeric vector of observed values
#' @param predicted Numeric vector of predicted quantile values
#' @param tau Quantile level (e.g., 0.5 for median)
#' @return Numeric scalar of mean pinball loss
exit_dqr_pinball_loss <- function(actual, predicted, tau) {
  residuals <- actual - predicted
  mean(ifelse(residuals >= 0, tau * residuals, (tau - 1) * residuals))
}

#' Pseudo-R² for quantile regression (Koenker-Machado)
#'
#' Calculates goodness-of-fit measure for quantile regression models.
#'
#' @param actual Numeric vector of observed values
#' @param predicted Numeric vector of predicted quantile values
#' @param tau Quantile level (e.g., 0.5 for median)
#' @return Numeric scalar between 0 and 1 (higher is better fit)
exit_dqr_pseudo_r2 <- function(actual, predicted, tau) {
  rho <- function(u, tau) sum(u * (tau - (u < 0)))
  rho_full <- rho(actual - predicted, tau)
  rho_null <- rho(actual - quantile(actual, tau), tau)
  1 - (rho_full / rho_null)
}

#' Coverage probability for quantile predictions
#'
#' Proportion of observations at or below the predicted quantile.
#'
#' @param actual Numeric vector of observed values
#' @param predicted Numeric vector of predicted quantile values
#' @return Numeric scalar representing coverage probability
exit_dqr_coverage <- function(actual, predicted) {
  mean(actual <= predicted)
}

#' Select quantile based on decay curve
#'
#' Maps decay weights to appropriate quantiles for exit analysis.
#'
#' @param t_norm Normalized time between 0 and 1
#' @param q Numeric vector of quantile values to select from
#' @param method Decay method: "gaussian", "laplace" (default), or "half-cosine"
#' @return Named numeric vector of selected quantiles (NA if no valid quantile)
exit_dqr_q <- function(t_norm, taus, method = "laplace") {
  sapply(exit_dqr_dc(t_norm, method = method), function(yi) {
    qs <- taus[taus < yi]
    if (length(qs) == 0)
      return(NA_real_)

    val <- max(qs)
    label <- paste0("q", sprintf("%.0f", val * 100))
    val <- setNames(val, label)
    val
  })
}

##
## Position helpers
##

#' Decay curve for decaying quantile regression
#'
#' Computes time-decay weights for exit analysis using various decay methods.
#'
#' @param t_norm Normalized time between 0 and 1
#' @param method Decay method: "gaussian", "laplace" (default), or "half-cosine"
#' @param ... Additional arguments (currently unused)
#' @return Numeric decay weight between 0 and 1
exit_dqr_dc <- function(t_norm, method = "laplace", alpha = .4) {
  if (method == "gaussian") {
    exp(-t_norm^2 / 2)
  } else if (method == "laplace") { # Double-exponential
    exp(-sqrt(alpha * t_norm))
  } else if (method == "half-cosine") {
    0.5 * (1 + cos(pi * t_norm / 2))
  }
}

#' Extract quantile values from fitted DQR models
#'
#' @param dqr_fits Named list of fitted models with names matching 'qXX' (e.g., q92, q82)
#' @return Numeric vector of quantile values in descending order
exit_dqr_extract_quantiles <- function(dqr_fits) {
  if (!all(grepl("^q[0-9]{2}$", names(dqr_fits))))
    stop("All dqr_fits elements must match pattern 'qXX' where XX are 2 digits")

  qnames <- rev(sort(names(dqr_fits)))
  as.numeric(sub("q", "", qnames)) / 100
}

#' Compute quantile weights based on decay curve
#'
#' Calculates probability weights for each quantile using a decay curve approach
#' with Gaussian density functions centered at each quantile.
#'
#' @param t_norm Numeric vector of normalized time values between 0 and 1
#' @param taus Numeric vector of quantile values in descending order (e.g., c(0.92, 0.82, 0.32))
#' @param method Decay method: "gaussian", "laplace" (default), or "half-cosine"
#' @param ... Additional arguments passed to exit_dqr_dc
#' @return Tibble with quantile probability columns (q92, q82, etc.) and t_norm as rownames
exit_dqr_weighted_probs <- function(t_norm, taus, method = "laplace", ...) {
  if (!all(taus == rev(sort(taus))))
    stop("taus must be in descending order")

  decay_curve <- exit_dqr_dc(t_norm, method = method, ...)
  sds <- rep(abs(mean(diff(c(1, taus)))), length(taus))^2
  
  # Create column names based on taus
  col_names <- paste0("q", sprintf("%.0f", taus * 100))
  
  # Calculate densities for all t_norm values
  result_matrix <- t(sapply(decay_curve, function(t) {
    densities <- sapply(
      seq_along(taus),
      \(i) dnorm(t, mean = taus[i], sd = sds[i])
    )
    densities <- densities / sum(densities)
    densities
  }))
  
  # Convert to data frame with proper column names and rownames
  result <- as.data.frame(result_matrix)
  colnames(result) <- col_names
  rownames(result) <- as.character(t_norm)
  
  # Convert to tibble while preserving rownames
  tibble::as_tibble(result, rownames = NA)
}
