exit_accuracy <- function(dfsr, drift = 0, side = c("long", "short")) {
  if (length(side) > 1) {
    stop("Please specify either 'long' or 'short' for the side parameter.")
  }

  result <- dfsr %>%
    dplyr::mutate(
      # Root Mean Square Error between predicted and actual return.
      rmse = sqrt((y - R - 1)^2)
    )

  if (side == "long") {
    result <- result %>%
      dplyr::mutate(
        # Proportion of the actual top captured by the exit.
        long_capture = ifelse(is.finite(y) & is.finite(R) & y != 1, R / (y - 1), NA),
        # Since the position follows a Brownian motion with drift,
        # alpha is the gain above the drift.
        long_alpha = long_capture - drift
      )
  } else if (side == "short") {
    result <- result %>%
      dplyr::mutate(
        # Proportion of the actual top captured by the exit.
        short_capture = ifelse(is.finite(y) & is.finite(R) & y != 1, R / (1 - y), NA),
        # Since the position follows a Brownian motion with drift,
        # alpha is the gain above the drift.
        short_alpha = short_capture - drift
      )
  }

  result
}

exit_metrics <- function(accuracy, side = c("long", "short")) {
  if (length(side) > 1) {
    stop("Please specify either 'long' or 'short' for the side parameter.")
  }

  result <- accuracy %>%
    dplyr::summarise(
      n = dplyr::n(),
      rmse = mean(rmse, na.rm = TRUE),
      t_mean = mean(t, na.rm = TRUE),
      t_sd = sd(t, na.rm = TRUE)
    )

  if (side == "long") {
    result <- result %>%
      dplyr::mutate(
        long_alpha = ifelse(
          length(accuracy$long_capture) > 0,
          mean(accuracy$long_alpha[is.finite(accuracy$long_alpha)], na.rm = TRUE),
          NA
        ),
        long_capture = ifelse(
          length(accuracy$long_capture) > 0,
          mean(accuracy$long_capture[is.finite(accuracy$long_capture)], na.rm = TRUE),
          NA
        ),
        long_capture_sd = ifelse(
          length(accuracy$long_capture) >= 2,
          sd(accuracy$long_capture[is.finite(accuracy$long_capture)], na.rm = TRUE),
          NA
        ),
        long_sharpe = sharpe_ratio(accuracy$R, na.rm = TRUE)
      )
  } else if (side == "short") {
    result <- result %>%
      dplyr::mutate(
        short_alpha = ifelse(
          length(accuracy$short_capture) > 0,
          mean(accuracy$short_alpha[is.finite(accuracy$short_alpha)], na.rm = TRUE),
          NA
        ),
        short_capture = ifelse(
          length(accuracy$short_capture) > 0,
          mean(accuracy$short_capture[is.finite(accuracy$short_capture)], na.rm = TRUE),
          NA
        ),
        short_capture_sd = ifelse(
          length(accuracy$short_capture) >= 2,
          sd(accuracy$short_capture[is.finite(accuracy$short_capture)], na.rm = TRUE),
          NA
        ),
        short_sharpe = sharpe_ratio(accuracy$R, na.rm = TRUE)
      )
  }

  result
}

#' Calculate Maximum Drawdown
#'
#' Computes the maximum drawdown from a series of returns.
#' Maximum drawdown is the largest peak-to-trough decline in cumulative portfolio value.
#'
#' @param returns A numeric vector of returns
#' @param na.rm Logical, whether to remove NA values (default: TRUE)
#' @return A single numeric value representing the maximum drawdown (as a negative percentage)
max_drawdown <- function(returns, na.rm = TRUE) {
  if (na.rm) {
    returns <- returns[!is.na(returns)]
  }

  if (length(returns) == 0) {
    return(NA_real_)
  }

  # Calculate cumulative returns (portfolio value over time)
  cum_returns <- cumprod(1 + returns)

  # Calculate running maximum
  running_max <- cummax(cum_returns)

  # Calculate drawdown at each point
  drawdown <- (cum_returns - running_max) / running_max

  # Return the maximum (most negative) drawdown
  min(drawdown, na.rm = TRUE)
}

#' Summarize Returns by Exit Method
#'
#' Computes summary statistics of returns (R) grouped by exit method.
#'
#' @param dfsr A data frame containing columns 'exit_method' and 'R' (returns)
#' @return A tibble with summary statistics (n, mean, sd, se, median, iqr, quantiles,
#' probabilities, expected values) per exit method
exit_methods_summary <- function(dfsr) {
  total <- nrow(dfsr)
  total_non_na <- sum(!is.na(dfsr$exit_method))

  dfsr %>%
    dplyr::group_by(exit_method) %>%
    dplyr::summarise(
      n                       = dplyr::n(),
      mean_R                  = mean(R, na.rm = TRUE),
      sd_R                    = sd(R, na.rm = TRUE),
      se_R                    = sd(R, na.rm = TRUE) / sqrt(dplyr::n()),
      median_R                = median(R, na.rm = TRUE),
      iqr_R                   = IQR(R, na.rm = TRUE),
      min_R                   = min(R, na.rm = TRUE),
      q05_R                   = quantile(R, 0.05, na.rm = TRUE),
      q32_R                   = quantile(R, 0.32, na.rm = TRUE),
      q68_R                   = quantile(R, 0.68, na.rm = TRUE),
      q95_R                   = quantile(R, 0.95, na.rm = TRUE),
      max_R                   = max(R, na.rm = TRUE),
      .groups                 = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      overall_probability     = n / total,
      capture_probability     = ifelse(is.na(exit_method), 0, n / total_non_na),
      expected                = mean_R * overall_probability,
      expected_given_capture  = mean_R * capture_probability
    )
}
