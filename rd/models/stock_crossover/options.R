df <- dfsr

K_values <- seq(.6, 1.4, by = .01)
maturity_values <- c(4, 14, 21, 28, 35, 42, 49)

options_surface_grid <- options_optim_surface_grid(K_values, maturity_values)
options_surface_goal <- options_optim_surface_matrix(
  options_surface_grid,
  length(K_values),
  length(maturity_values)
)

plotly::plot_ly() %>%
  plotly::add_surface(
    x = K_values,
    y = maturity_values,
    z = options_surface_goal,
    colorscale = "Viridis"
  ) %>%
  plotly::layout(
    title = NULL,
    scene = list(
      xaxis = list(title = "Strike Multiplier (K)"),
      yaxis = list(title = "Days to Maturity (tm)"),
      zaxis = list(
        title = "Portfolio Growth",
        range = c(
          min(options_surface_grid$result, na.rm = TRUE, 0),
          max(options_surface_grid$result, na.rm = TRUE)
        )
      )
    )
  )

# Find optimal parameters
print(options_optim_maximal(options_surface_grid))
