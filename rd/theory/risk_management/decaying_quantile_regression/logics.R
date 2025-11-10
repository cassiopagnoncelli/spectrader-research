# DQR models quantiles
q <- c(.92, .86, .82, .32)

# Time (scaled)
x <- seq(0, 1, by = 0.01)

# Transformation function
# y <- 0.5 * (1 + cos(pi * x / 2))    # Half-cosine
y <- exp(-sqrt(.07 * x))        # Laplace
# y <- exp(-x^2 / 2)              # Gaussian

# Quantile applied over time
z <- sapply(y, function(yi) {
  qs <- q[q < yi]
  if (length(qs) == 0) return(NA_real_)
  max(qs)
})

df <- tibble(x = x, y = y, z = z)

ggplot(df, aes(x = x)) +
  geom_line(aes(y = y), color = "blue", linewidth = 1.1) +
  geom_line(aes(y = z), color = "red", linewidth = 1, linetype = "dashed") +
  geom_hline(yintercept = seq(0, 1, by = 0.05), color = "gray80", linetype = "dashed") +
  geom_vline(xintercept = seq(0, 1, by = 0.05), color = "gray80", linetype = "dashed") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal() +
  labs(
    title = "Decaying Quantile Regression Threshold",
    x = "Time",
    y = "Quantile Catch"
  )
