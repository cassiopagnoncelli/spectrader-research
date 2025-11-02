library("RQuantLib")
library("plotly")

# Function to compute American option price.
option_fun <- function(T, S, K = 120) {
  AmericanOption(
    "call",
    underlying = S,
    strike = K,
    dividendYield = 0,
    riskFreeRate = 0.0409,
    maturity = T / 365,
    volatility = 1
  )$value / AmericanOption(
    "call",
    underlying = 100,
    strike = K,
    dividendYield = 0,
    riskFreeRate = 0.0409,
    maturity = 85 / 365,
    volatility = 1
  )$value - 1
}

# Grid.
maturities <- seq(1, 90, by = 1)
prices     <- seq(70, 140, by = 1)
opt_profit <- outer(maturities, prices, Vectorize(option_fun))

# Plot.
plot_ly(
  x = prices,
  y = maturities,
  z = opt_profit,
  type = "surface",
  colorscale = "Viridis",
  contours = list(
    z = list(show = TRUE, usecolormap = TRUE, highlightcolor = "white", project = list(z = TRUE))
  )
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Strike", tickvals = seq(70, 130, 10)),
      yaxis = list(title = "Maturity (days)", tickvals = seq(0, 90, 15)),
      zaxis = list(title = "Option Price", range = c(0, max(opt_profit))),
      aspectmode = "manual",
      aspectratio = list(x = 1, y = 1, z = 0.5),   # 👈 balance proportions
      camera = list(eye = list(x = 1.5, y = 1.5, z = 0.8)) # 👁️ better viewing angle
    )
  )
