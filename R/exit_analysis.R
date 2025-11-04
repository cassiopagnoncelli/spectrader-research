exit_accuracy <- function(dfsr, drift = 0) {
  dfsr %>%
    dplyr::mutate(
      # Root Mean Square Error between predicted and actual return.
      rmse = sqrt((y - R - 1)^2),
      # Proportion of the actual top captured by the exit.
      long_capture = R / (y - 1),
      short_capture = R / (1 - y),
      # Since the position follows a Brownian motion with drift,
      # alpha is the gain above the drift.
      long_alpha = long_capture - drift,
      short_alpha = short_capture - drift
    )
}

exit_metrics <- function(accuracy) {
  tibble::tibble(
    n = nrow(accuracy),
    rmse = mean(accuracy$rmse, na.rm = TRUE),
    t_mean = mean(accuracy$t, na.rm = TRUE),
    t_sd = sd(accuracy$t, na.rm = TRUE),
    long_alpha = mean(accuracy$long_alpha, na.rm = TRUE),
    long_capture = mean(accuracy$long_capture, na.rm = TRUE),
    long_capture_sd = sd(accuracy$long_capture, na.rm = TRUE),
    long_sharpe = mean(accuracy$long_alpha, na.rm = TRUE) / long_capture_sd * sqrt(252),
    short_alpha = mean(accuracy$short_alpha, na.rm = TRUE),
    short_capture = mean(accuracy$short_capture, na.rm = TRUE),
    short_capture_sd = sd(accuracy$short_capture, na.rm = TRUE),
    short_sharpe = mean(accuracy$short_alpha, na.rm = TRUE) / short_capture_sd * sqrt(252)
  )
}
