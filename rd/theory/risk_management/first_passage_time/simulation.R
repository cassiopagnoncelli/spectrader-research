library(ggplot2)

K <- 1
r <- 0.03
mu <- 0.0030 # from your estimate
sigma <- 0.075

T <- 1
t_grid <- seq(0, T, length.out = 100)
sig_grid <- seq(0.02, 0.15, length.out = 100)

calc_Sstar <- function(mu, sigma, r, K, t) {
  lambda <- 0.5 * (1 - t / T) * r # small time penalty
  beta <- 0.5 - mu / sigma^2 + sqrt((mu / sigma^2 - 0.5)^2 + 2 * (r + lambda) / sigma^2)
  K * beta / (beta - 1)
}

df <- expand.grid(t = t_grid, sigma = sig_grid)
df$Sstar <- mapply(calc_Sstar, mu, df$sigma, r, K, df$t)

ggplot(df, aes(t, sigma, z = Sstar)) +
  geom_contour_filled() +
  labs(
    title = "Optimal Exit Surface S*(t, σ)",
    x = "Time (years elapsed)",
    y = "Volatility",
    fill = "S* (sell multiple)"
  ) +
  theme_minimal()
