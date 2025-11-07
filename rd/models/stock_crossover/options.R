df <- dfsr

optprice <- function(S_0, S_t, t, vol) {
  mapply(function(s0, st, tm, v) {
    AmericanOption(
      "call",
      underlying = s0,
      strike = st,
      dividendYield = 0,

      # Annual risk-free rate
      riskFreeRate = 0.0409,

      # Days in year
      maturity = tm / 365,

      # Implied volatility
      volatility = v
    )$value
  }, S_0, S_t, t, vol)
}

K <- 1.2
maturity <- 15
df %>% slice_head(n = 10) %>%
  dplyr::mutate(
    option_entry = optprice(1, K, maturity, 1),
    option_exit = ifelse(t < maturity - 1, optprice(1 + R, K, maturity - t - 1, 1.1), 0),
    option_return = (option_exit - option_entry) / option_entry
  ) %>%
  dplyr::summarise(overall = log10(prod(1 + .3 * option_return)))

goal <- function(K, maturity, kelly = .3, data = df) {
  data %>% slice_head(n = 10) %>%
    dplyr::mutate(
      option_entry = optprice(1, K, maturity, 1),
      option_exit = ifelse(t < maturity, optprice(1 + R, K, maturity - t - 1, 1.1), 0),
      option_return = (option_exit - option_entry) / option_entry
    ) %>%
    dplyr::summarise(overall = log10(prod(1 + kelly * option_return))) %>%
    dplyr::pull(overall)
}

# Optimization across K x maturity space
K_values <- seq(1.02, 1.2, by = .01)
maturity_values <- c(7, 15, 22, 44)

# Create parameter grid
param_grid <- expand.grid(K = K_values, maturity = maturity_values)

# Evaluate goal function for each combination
param_grid$result <- mapply(goal, param_grid$K, param_grid$maturity)

# Find optimal parameters
optimal <- param_grid[which.max(param_grid$result), ]

print("Optimal parameters:")
print(optimal)

# Visualize the results in 3D
library(plotly)
plot_ly(
  param_grid,
  x = ~K,
  y = ~maturity,
  z = ~result,
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 5,
    color = ~result,
    colorscale = "Viridis",
    showscale = TRUE
  )
) %>%
  add_trace(
    type = "mesh3d",
    opacity = 0.5
  ) %>%
  layout(
    title = "Goal Function Optimization",
    scene = list(
      xaxis = list(title = "Strike Multiplier (K)"),
      yaxis = list(title = "Maturity (days)"),
      zaxis = list(
        title = "Log Return (Goal)",
        range = c(min(param_grid$result, 0), max(param_grid$result))
      )
    )
  )
