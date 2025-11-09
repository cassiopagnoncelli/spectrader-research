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
