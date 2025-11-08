K_values <- seq(.6, 1.4, by = .01)
tm_values <- c(4, 14, 21, 28, 35, 42, 49)

options_surface_grid <- options_optim_surface_grid(
  data = dfsr,
  K_values = K_values,
  tm_values = tm_values,
  # goal = list(method = "sharpe"),
  # goal = list(method = "log-portfolio-kelly", q = .5),
  goal = list(method = "log-portfolio", wager = .05),
  vol_0 = 0.9,
  vol_t = 0.9
)

options_surface_goal <- options_optim_surface_matrix(
  options_surface_grid,
  length(K_values),
  length(tm_values)
)

plotly::plot_ly() %>%
  plotly::add_surface(
    x = K_values,
    y = tm_values,
    z = options_surface_goal,
    colorscale = "Viridis"
  ) %>%
  plotly::layout(
    title = NULL,
    scene = list(
      xaxis = list(title = "Strike Multiplier (K)"),
      yaxis = list(title = "Days to Maturity (tm)"),
      zaxis = list(
        title = "Portfolio Goal",
        range = c(
          min(options_surface_grid$result, na.rm = TRUE, 0),
          max(options_surface_grid$result, na.rm = TRUE)
        )
      )
    )
  )

# Find optimal parameters
print(options_optim_maximal(options_surface_grid))

# Returns
american_optprice_returns(dfsr, K = 1.2, tm = 30) %>%
  pull(opt_R) %>%
  kelly_quantile(q = .25)
