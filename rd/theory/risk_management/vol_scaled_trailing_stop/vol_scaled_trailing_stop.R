library(dplyr)
library(ggplot2)
library(zoo)

set.seed(123)

# --- Parameters -------------------------------------------------------------
n <- 500
mu <- 0.0005
sigma <- 0.02
S0 <- 100
n_vol <- 20
k <- 2.5

# --- Generate log-normal path ----------------------------------------------
logS <- log(S0) + cumsum(rnorm(n, mean = mu, sd = sigma))
S <- exp(logS)
df <- data.frame(t = 1:n, S = S)

# --- Compute trailing stop --------------------------------------------------
df <- df %>%
  mutate(
    logret = c(NA, diff(log(S))),
    vol = rollapply(logret, n_vol, sd, fill = NA, align = "right"),
    Smax = cummax(S),
    stop = Smax * exp(-k * vol)
  ) %>%
  # Drop warm-up region where vol is NA
  filter(!is.na(vol)) %>%
  mutate(
    t = t - min(t) + 1, # reset timeline to start at 1
    exit = S < stop & lag(S, default = first(S)) >= lag(stop, default = first(stop))
  )

# --- Plot -------------------------------------------------------------------
ggplot(df, aes(t, S)) +
  geom_line(color = "black") +
  geom_line(aes(y = stop), color = "red", linetype = "dashed") +
  geom_point(data = subset(df, exit), aes(y = S), color = "red", size = 2) +
  labs(
    title = "Volatility-Scaled Trailing Stop (Backtest Starts After Volatility Defined)",
    x = "Time", y = "Price"
  ) +
  theme_minimal()
