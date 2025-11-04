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
    ) %>%
    print(n = 100)
}

exit_metrics <- function(accuracy) {
  tibble::tibble(
    rmse = mean(accuracy$rmse, na.rm = TRUE),
    alpha_captured_mean = mean(accuracy$alpha_captured, na.rm = TRUE),
    alpha_captured_median = median(accuracy$alpha_captured, na.rm = TRUE)
  )
}
