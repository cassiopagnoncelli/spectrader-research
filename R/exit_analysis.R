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
        long_capture = R / (y - 1),
        # Since the position follows a Brownian motion with drift,
        # alpha is the gain above the drift.
        long_alpha = long_capture - drift
      )
  } else if (side == "short") {
    result <- result %>%
      dplyr::mutate(
        # Proportion of the actual top captured by the exit.
        short_capture = R / (1 - y),
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
      sharpe = sharpe_ratio(R),
      rmse = mean(rmse, na.rm = TRUE),
      t_mean = mean(t, na.rm = TRUE),
      t_sd = sd(t, na.rm = TRUE)
    )
  
  if (side == "long") {
    result <- result %>%
      dplyr::mutate(
        long_alpha = mean(accuracy$long_alpha, na.rm = TRUE),
        long_capture = mean(accuracy$long_capture, na.rm = TRUE),
        long_capture_sd = sd(accuracy$long_capture, na.rm = TRUE),
        long_sharpe = long_alpha / long_capture_sd * sqrt(252)
      )
  } else if (side == "short") {
    result <- result %>%
      dplyr::mutate(
        short_alpha = mean(accuracy$short_alpha, na.rm = TRUE),
        short_capture = mean(accuracy$short_capture, na.rm = TRUE),
        short_capture_sd = sd(accuracy$short_capture, na.rm = TRUE),
        short_sharpe = short_alpha / short_capture_sd * sqrt(252)
      )
  }
  
  result
}
