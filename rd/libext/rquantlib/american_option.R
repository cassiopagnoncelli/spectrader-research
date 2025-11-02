library(RQuantLib)

# Calculate option price using the Barone-Adesi and Whaley approximation method
# for American options.
AmericanOption(
  "call",
  underlying = 130.82,
  strike = 165.00,
  dividendYield = 0,

  # Annual risk-free rate
  riskFreeRate = 0.0409,

  # Days in year
  maturity = 76 / 365,

  # Implied volatility
  volatility = 1.0524
)

# Calculate Implied Volatility, which is a key parameter in AmericanOption.
AmericanOptionImpliedVolatility(
  "call",
  value = 14.5,
  underlying = 130.82,
  strike = 165.00,
  dividendYield = 0,
  riskFreeRate = 0.0409,
  maturity = 76 / 365,
  volatility = 0.7          # initial guess
)
