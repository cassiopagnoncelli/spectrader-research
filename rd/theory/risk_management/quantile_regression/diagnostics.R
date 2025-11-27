library(quantreg)
library(dplyr)
library(ggplot2)

qr_diagnostics_grid <- function(form, data, taus = seq(0.1, 0.95, by = 0.05), plot = TRUE) {
  results <- lapply(taus, function(tau) {
    fit <- rq(form, tau = tau, data = data)
    pred <- predict(fit, data)
    actual <- data$S

    # Quantile loss
    rho <- function(u, tau) sum(u * (tau - (u < 0)))
    rho_full <- rho(actual - pred, tau)
    rho_null <- rho(actual - quantile(actual, tau), tau)
    R2_pseudo <- 1 - rho_full / rho_null

    pinball_loss <- mean((tau - (actual < pred)) * (actual - pred))
    coverage <- mean(actual <= pred)

    data.frame(tau, R2_pseudo, pinball_loss, coverage)
  }) %>% bind_rows()

  if (plot) {
    p1 <- ggplot(results, aes(tau, R2_pseudo)) +
      geom_line(linewidth = 1) +
      geom_point() +
      labs(title = "Pseudo-R² across quantiles", y = "Pseudo-R²", x = "τ") +
      theme_minimal()

    p2 <- ggplot(results, aes(tau, pinball_loss)) +
      geom_line(linewidth = 1) +
      geom_point() +
      labs(title = "Pinball Loss", y = "Loss", x = "τ") +
      theme_minimal()

    p3 <- ggplot(results, aes(tau, coverage)) +
      geom_line(linewidth = 1) +
      geom_point() +
      geom_hline(aes(yintercept = taus), color = "red", linetype = "dotted", alpha = 0.3) +
      labs(title = "Coverage vs Quantile Level", y = "Empirical Coverage", x = "τ") +
      theme_minimal()

    print(p1)
    print(p2)
    print(p3)
  }

  return(results)
}

# Example use:
results <- qr_diagnostics_grid(form, train_df, taus = seq(0.1, 0.95, by = 0.05))
results
