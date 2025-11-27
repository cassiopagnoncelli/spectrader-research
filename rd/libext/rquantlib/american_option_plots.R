library("RQuantLib")
library("plotly")

# Function to compute American option price.
option_fun <- function(maturity, strike) {
  AmericanOption(
    "call",
    underlying = 100,
    strike = strike,
    dividendYield = 0,
    riskFreeRate = 0.0409,
    maturity = maturity / 365,
    volatility = 1
  )$value
}

# Grid.
maturities <- seq(1, 90, by = 1)
strikes <- seq(70, 130, by = 1)
option_prices <- outer(maturities, strikes, Vectorize(option_fun))

# Plot.
plot_ly(
  x = strikes,
  y = maturities,
  z = option_prices,
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
      zaxis = list(title = "Option Price", range = c(0, max(option_prices))),
      aspectmode = "manual",
      aspectratio = list(x = 1, y = 1, z = 0.5), # 👈 balance proportions
      camera = list(eye = list(x = 1.5, y = 1.5, z = 0.8)) # 👁️ better viewing angle
    )
  )
