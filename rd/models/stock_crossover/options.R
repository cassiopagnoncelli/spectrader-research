K_values <- seq(.6, 1.4, by = .01)

tm_values <- tibble(x = c(4, 14, 21, 28, 35, 42, 49)) %>%
  dplyr::filter(x > ceiling(365/252 * feature_days)) %>%
  dplyr::pull(x)

options_surface_grid <- options_optim_surface_grid(
  data = dfsr,
  K_values = K_values,
  tm_values = tm_values,
  # goal = list(method = "sharpe"),
  # goal = list(method = "log-portfolio-kelly", q = .3, cap = .4),
  goal = list(method = "log-portfolio", wager = .08),
  vol_0 = NA,
  vol_t = NA
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

# Returns
opt_R <- american_optprice_returns(dfsr, K = 1.25, tm = 21) %>% pull(opt_R)
tibble(
  f_star = kelly_fraction(opt_R),
  f_star_q = kelly_quantile(opt_R, tau = 0.3, cap = .4),
  log_portfolio = log(prod(1 + f_star * opt_R)),
  log_portfolio_q = log(prod(1 + f_star_q * opt_R))
)

print(options_optim_maximal(options_surface_grid)) # Maximum
