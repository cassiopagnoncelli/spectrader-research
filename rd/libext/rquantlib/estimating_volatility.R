fetl <- Fetl$new()

q <- fetl$send_query("
  SELECT q.date, q.close
  FROM quotes q
  JOIN companies c ON q.company_id = c.id
  WHERE c.symbol = 'TSLA'
    AND q.date between current_date - interval '1 year' and current_date
") %>% tibble

logrets <- log(q$close / lag(q$close))
n <- 252
sd_impl <- sqrt(n) * sd(tail(logrets, n), na.rm = T)
sd_impl

AmericanOption(
  "call",
  underlying = 456.56,
  strike = 505.00,
  dividendYield = 0,

  # Annual risk-free rate
  riskFreeRate = 0.0409,

  # Days in year
  maturity = 76 / 365,

  # Implied volatility
  volatility = .579
)

library(rugarch)

# --- Compute log returns ---
rets <- diff(log(q$close))
rets <- rets[!is.na(rets)]

# --- Fit standard GARCH(1,1) ---
spec <- ugarchspec(
  mean.model = list(armaOrder = c(2,0)),
  variance.model = list(model = "sGARCH", garchOrder = c(3,2)),
  distribution.model = "norm"
)

fit <- ugarchfit(spec, data = rets)

# --- Extract most recent conditional volatility ---
sigma_last <- tail(sigma(fit), 1)        # last fitted daily σ
vol_est <- sqrt(252) * as.numeric(sigma_last)  # annualised σ

vol_est
