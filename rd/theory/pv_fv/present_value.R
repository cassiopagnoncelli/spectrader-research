# Load tibble library
library(tibble)

# Disable scientific notation
options(scipen = 999)

# Investment parameters
cagr <- 0.3 # 30% annual return
magr <- (1 + cagr)**(1 / 12) - 1 # Monthly growth rate
n <- 12 * 15 # 180 months (15 years) - total period
m <- 24 # m months (m/12 years) - investment period
monthly_investment <- 100000

# Initialize tibble
investment_table <- tibble(
  month_number = 1:n,
  new_investment = c(rep(monthly_investment, m), rep(0, n - m)),
  past_month_net_worth = numeric(n),
  month_return = numeric(n),
  this_month_net_worth = numeric(n)
)

# Calculate month by month
for (i in 1:n) {
  if (i == 1) {
    # First month
    investment_table$past_month_net_worth[i] <- 0
    investment_table$month_return[i] <- 0
    investment_table$this_month_net_worth[i] <- investment_table$new_investment[i]
  } else {
    # Subsequent months
    investment_table$past_month_net_worth[i] <- investment_table$this_month_net_worth[i - 1]
    investment_table$month_return[i] <- investment_table$past_month_net_worth[i] * magr
    investment_table$this_month_net_worth[i] <- investment_table$past_month_net_worth[i] +
      investment_table$month_return[i] +
      investment_table$new_investment[i]
  }
}

# Calculate totals
future_value <- tail(investment_table$this_month_net_worth, 1)
total_invested <- monthly_investment * m
total_returns <- future_value - total_invested

# Display first 10 rows
print(investment_table, n = 10)

# Display rows around the transition (last investment month and first growth-only months)
cat("\n... Transition from investing to growth-only ...\n\n")
print(investment_table[(m - 2):(m + 3), ])

# Display last 10 rows
cat("\n... Final months ...\n\n")
print(tail(investment_table, 10))

# Display summary in one consolidated output
cat(sprintf(
  "

Investment Analysis
===================

Monthly Investment:         %s
Investment Period:          %d years (%d months)
Growth-Only Period:         %d years (%d months)
Total Period:               %d years (%d months)
Annual Growth Rate (CAGR):  %.1f%%
Monthly Growth Rate:        %.4f%%

Total Invested:             %s
Final Net Worth:            %s
Total Returns:              %s
Return Multiple:            %.2fx

",
  format(monthly_investment, big.mark = ",", scientific = FALSE),
  m / 12, m,
  (n - m) / 12, (n - m),
  n / 12, n,
  cagr * 100,
  magr * 100,
  format(round(total_invested), big.mark = ",", scientific = FALSE),
  format(round(future_value), big.mark = ",", scientific = FALSE),
  format(round(total_returns), big.mark = ",", scientific = FALSE),
  future_value / total_invested
))
