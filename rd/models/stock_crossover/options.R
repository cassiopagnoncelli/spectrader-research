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

goal <- function(K, maturity, vol_0 = .3, vol_t = 1, log.transform = TRUE, data = df) {
  kelly <- kelly_quantile(na.omit(data$R), tau = 0.8)
  data %>% slice_head(n = 10) %>%
    dplyr::mutate(
      option_entry = optprice(1, K, maturity, vol_0),
      option_exit = ifelse(t < maturity, optprice(1 + R, K, maturity - t - 1, vol_t), 0),
      option_return = (option_exit - option_entry) / option_entry
    ) %>%
    dplyr::summarise(W = prod(1 + kelly * option_return), w = log10(W)) %>%
    dplyr::pull(ifelse(log.transform, w, W))
}

# Optimization across K x maturity space
K_values <- seq(1.02, 1.3, by = .01)
maturity_values <- seq(4, 80, by = 3)

# Create parameter grid
param_grid <- expand.grid(K = K_values, maturity = maturity_values)

# Evaluate goal function for each combination
param_grid$result <- mapply(goal, param_grid$K, param_grid$maturity)

# Visualize the results in 3D surface
# Reshape data into matrix for surface plot
# Matrix needs to be transposed: rows = maturity, columns = K
result_matrix <- matrix(
  param_grid$result,
  nrow = length(K_values),
  ncol = length(maturity_values)
)
result_matrix <- t(result_matrix)  # Transpose so rows = maturity, cols = K

plotly::plot_ly(
  x = K_values,
  y = maturity_values,
  z = result_matrix,
  type = "surface",
  colorscale = "Viridis"
) %>%
  plotly::layout(
    title = "Goal Function Optimization",
    scene = list(
      xaxis = list(
        title = "Strike Multiplier (K)",
        range = c(min(K_values), max(K_values))
      ),
      yaxis = list(
        title = "Maturity (days)",
        range = c(min(maturity_values), max(maturity_values))
      ),
      zaxis = list(
        title = "Portfolio Growth",
        range = c(min(param_grid$result, 0), max(param_grid$result))
      )
    )
  )

# Find optimal parameters
optimal <- param_grid[which.max(param_grid$result), ]

print("Optimal parameters:")
print(optimal)
