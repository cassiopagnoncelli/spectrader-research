#' @importFrom magrittr %>%
NULL

#' Train quantile regression models for exit analysis
#'
#' Fits multiple quantile regression models using position cohort data.
#'
#' @param signals Signal data for position cohort generation
#' @param quotes Quotes data
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
train_dqr <- function(signals, quotes, taus, formulas, max_position_days = 60) {
  if (length(taus) != length(formulas))
    stop("taus and formulas must have the same length")

  # Positions cohorts enriched for decaying quantile regression features
  posl <- position_cohorts(
    signals,
    before_days = 30,
    after_days = max_position_days,
    quotes = quotes
  )

  # Amalgamate all positions into a single data frame
  data <- purrr::map_dfr(seq_along(posl), \(i) {
    posl[[i]] %>%
      dplyr::filter(t > 0) %>%
      dplyr::mutate(position_id = i, t_norm = t / max(t)) %>%
      dplyr::select(-c(symbol, date, close, fets::fwd_methods())) %>%
      na.omit()
  })

  models <- purrr::map2(
    formulas, taus,
    \(formula, tau) quantreg::rq(formula, tau = tau, data = data)
  )
  names(models) <- paste0("q", sprintf("%02.0f", taus * 100))
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
exit_dqr_dc <- function(t_norm, method = "laplace", alpha = .15) {
  if (method == "identity") {
    t_norm
  } else if (method == "gaussian") {
    exp(-t_norm^2 / 2)
  } else if (method == "laplace") { # Double-exponential
    exp(-sqrt(alpha * pmax(t_norm, 0)))
  } else if (method == "half-cosine") {
    0.5 * (1 + cos(pi * t_norm / 2))
  } else {
    stop("Unknown method: ", method)
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

#' Right-weighted energy metric
#'
#' Computes weighted sum with quadratic position-based weights favoring later elements.
#'
#' @param x Numeric vector
#' @return Numeric scalar of right-weighted energy
right_energy <- function(x) {
  w <- seq_along(x) / length(x)
  sum(w^2 * x)
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
exit_dqr_weighted_probs <- function(t_norm, vol_norm, taus, method = "laplace", ...) {
  if (!all(taus == rev(sort(taus))))
    stop("taus must be in descending order")
  if (is.null(t_norm) && is.null(vol_norm))
    stop("At least one of t_norm or vol_norm must be provided")

  sds <- rep(abs(mean(diff(c(1, taus)))), length(taus))^2

  # Create column names based on taus
  col_names <- paste0("q", sprintf("%.0f", taus * 100))

  # Calculate densities for all t_norm values
  if (!is.null(t_norm)) {
    decay_curve <- exit_dqr_dc(t_norm, method = method, ...)
    decay_matrix <- sapply(decay_curve, function(t) {
      densities <- sapply(
        seq_along(taus),
        \(i) dnorm(t, mean = taus[i], sd = sds[i])
      )
      densities <- densities / sum(densities)
      densities
    })
    if (is.null(vol_norm)) {
      final_matrix <- decay_matrix
    }
  }

  # Calculate all densities for vol_norm values
  if (!is.null(vol_norm)) {
    burst_curve <- exit_dqr_dc(vol_norm, method = "gaussian")
    burst_matrix <- sapply(burst_curve, function(v) {
      densities <- sapply(
        seq_along(taus),
        \(i) dnorm(v, mean = taus[i], sd = sds[i])
      )
      densities <- densities / sum(densities)
      densities
    })
    if (is.null(t_norm)) {
      final_matrix <- burst_matrix
    }
  }

  # Combine decay and burst matrices by selecting higher right energy
  if (!is.null(t_norm) && !is.null(vol_norm)) {
    final_matrix <- sapply(seq_len(ncol(decay_matrix)), function(i) {
      decay_right_energy <- right_energy(decay_matrix[, i])
      burst_right_energy <- right_energy(burst_matrix[, i])
      col <- if (!is.na(burst_right_energy) && burst_right_energy > decay_right_energy) {
        burst_matrix[, i]
      } else {
        decay_matrix[, i]
      }
      col
    })
  }

  # Convert to data frame with proper column names
  result <- final_matrix %>% t() %>% as.data.frame()
  colnames(result) <- col_names

  # Convert to tibble (row names not needed since they won't be preserved)
  tibble::as_tibble(result)
}

#' Evaluate DQR models and generate exit signals
#'
#' Decaying curve is leveled at extreme quantiles progressively lowering the quantiles
#' as time goes by with occasional bursts when vix levels are high.
#'
#' @param data Position cohort data frame with columns S, t, and model features
#' @param max_position_days Maximum days after position entry for normalization
#' @param side Position side: "long" or "short"
#' @param dqr_fits Named list of fitted quantile regression models from train_dqr()
#' @param history Include pre-entry historical data (t < 0) in output (default: FALSE)
#' @return Data frame with quantile predictions (qhat_*), weighted prediction (qhat), and exit signals
#' @examples
#' \dontrun{
#' exit_dqr_eval(position_data, 60, "long", my_dqr_fits)
#' }
exit_dqr_eval <- function(
  data,
  max_position_days,
  side,
  dqr_fits,
  enable_vol_bursts = TRUE,
  enable_time_decay = TRUE,
  minS = NA,
  minT = NA,
  history = FALSE,
  ...
) {
  qnames <- rev(sort(names(dqr_fits)))
  qhat_cols <- paste0("qhat_", qnames)
  exit_cols <- paste0("exit_", qnames)
  taus <- exit_dqr_extract_quantiles(dqr_fits)

  # Feature engineering
  result <- data %>%
    dplyr::filter(t >= ifelse(history, -Inf, 0)) %>%
    # t_norm is used for decay weighting in such a way the bound will lower with
    # higher vix levels and longer position durations.
    dplyr::mutate(
      t_norm = t / max_position_days,
      vix_norm = vix / 100 - .19458 - 0.078 / 2
    )

  # Generate predictions for all quantile fits
  for (i in seq_along(qnames)) {
    result[[qhat_cols[i]]] <- predict(dqr_fits[[qnames[i]]], result)
  }

  # Compute weighted combination of quantile predictions
  # M := matrix of all qhat predictions
  M <- as.matrix(result[qhat_cols])

  # w := weights from decay curve + vol bursts (one row per observation)
  w <- as.matrix(exit_dqr_weighted_probs(
    if (enable_time_decay) result$t_norm else NULL,
    if (enable_vol_bursts) result$vix_norm else NULL,
    taus,
    ...
  ))

  # qhat := row-wise weighted sum of quantile predictions
  result$qhat <- rowSums(M * w)

  # Create exit signals for each quantile
  for (i in seq_along(qnames)) {
    if (side == "long")
      result[[exit_cols[i]]] <- result$S >= result[[qhat_cols[i]]]
    else if (side == "short")
      result[[exit_cols[i]]] <- result$S <= result[[qhat_cols[i]]]
  }

  if (side == "long") {
    result$exit_dqr <- result %>%
      dplyr::mutate(
        exit_dqr = keep_first_true_only(
          S > qhat & S > max(1, minS, na.rm = TRUE) & t >= max(0, minT, na.rm = TRUE)
        )
      ) %>%
      dplyr::pull(exit_dqr)
  } else if (side == "short") {
    result$exit_dqr <- result %>%
      dplyr::mutate(
        exit_dqr = keep_first_true_only(
          S < qhat & S < max(1, minS, na.rm = TRUE) & t >= max(0, minT, na.rm = TRUE)
        )
      ) %>%
      dplyr::pull(exit_dqr)
  }

  # Combine all exit signals
  result$exit <- result$exit | (!is.na(result$exit_dqr) & result$exit_dqr)

  result %>%
    dplyr::mutate(dqr_line = ifelse(t < minT, NA, qhat)) %>%
    dplyr::select(-dplyr::all_of(c(qhat_cols, exit_cols, "qhat")))
}
