library("tibble")
library("ggplot2")
library("dplyr")

# Black-Scholes Put Option Pricing
bs_put <- function(S, K, r = 0.05, T = 63/252, sigma = 0.2) {
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  put_price <- K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
  return(put_price)
}

S <- 100  # Current stock price
K <- 90   # Strike price
sigma <- .2  # Volatility

s <- seq(110, 65, by = -0.25)
k <- sapply(s, function(x) bs_put(S, K = x, sigma = sigma))

# plot(x = s, y = k, type = 'l', col = 'blue', lwd = 2,
#      ylim = c(0, max(k)),
#      xlab = 'Strike Price (K)', ylab = 'Put Option Price',
#      main = 'Black-Scholes Put Option Price vs Strike Price')
# abline(h = 0)

# Long put simulation
premium <- bs_put(S, K, sigma = sigma)
premium
put_prices <- sapply(s, function(x) bs_put(x, K, T = 15/252, sigma = sigma))
put_prices

long_put <- tibble(
  premium = premium,
  s = s,
  put_price = put_prices,
  intrinsic_value = pmax(K - s, 0), # How much it is ITM
  extrinsic_value = put_prices - intrinsic_value,
  put_sell_roi = (put_prices - premium) / premium * 100,
  exercise_roi = (intrinsic_value - premium) / premium * 100
) %>% arrange(-exercise_roi)
long_put

ggplot(long_put, aes(x = s)) +
  geom_line(aes(y = put_sell_roi, color = "Sell Put ROI")) +
  geom_line(aes(y = exercise_roi, color = "Exercise Put ROI")) +
  geom_line(aes(y = 0), linetype = "dashed", color = "black") +
  geom_vline(xintercept = S, linetype = "solid", color = "red") +
  geom_vline(xintercept = K, linetype = "dotted", color = "red") +
  labs(
    title = "Long Put Option ROI vs Stock Price at Expiry",
    x = "Stock Price at Expiry",
    y = "ROI (%)"
  ) +
  scale_color_manual(values = c("Sell Put ROI" = "blue", "Exercise Put ROI" = "orange")) +
  theme_minimal()
