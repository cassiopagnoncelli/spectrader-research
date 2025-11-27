library(dplyr)
library(zoo)
library(ggplot2)

set.seed(123)

# --- Parameters -------------------------------------------------------------
n_steps <- 250 # time steps per path
n_paths <- 2000 # Monte Carlo simulations
mu <- 0.0005 # drift (set 0 for pure Brownian)
sigma <- 0.02 # volatility
S0 <- 100
n_vol <- 20
k <- 2.5

simulate_path <- function() {
  logS <- log(S0) + cumsum(rnorm(n_steps, mean = mu, sd = sigma))
  S <- exp(logS)
  df <- data.frame(t = 1:n_steps, S = S)

  df <- df %>%
    mutate(
      logret = c(NA, diff(log(S))),
      vol = rollapply(logret, n_vol, sd, fill = NA, align = "right"),
      Smax = cummax(S),
      stop = Smax * exp(-k * vol),
      exit = S < stop & lag(S, default = first(S)) >= lag(stop, default = first(stop))
    )

  # --- Portfolio simulation -------------------------------------------------
  # assume always fully invested: compounded simple returns
  ret <- c(0, diff(S) / head(S, -1))
  portfolio <- cumprod(1 + ret)
  df$portfolio <- portfolio
  df$portfolio
}

# --- Run Monte Carlo --------------------------------------------------------
portfolios <- replicate(n_paths, simulate_path())
median_curve <- apply(portfolios, 1, median, na.rm = TRUE)
time <- 1:n_steps

# --- Prepare data for plotting ----------------------------------------------
df_plot <- data.frame(
  t = rep(time, n_paths),
  value = as.vector(portfolios)
)

df_median <- data.frame(t = time, median_value = median_curve)

# --- Plot -------------------------------------------------------------------
ggplot() +
  geom_line(
    data = df_plot, aes(x = t, y = value, group = rep(1:n_paths, each = n_steps)),
    color = "gray70", alpha = 0.25
  ) +
  geom_line(
    data = df_median, aes(x = t, y = median_value),
    color = "red", linewidth = 1.2
  ) +
  labs(
    title = "Monte Carlo Simulation of Volatility-Scaled Trailing-Stop Strategy",
    subtitle = "Gray: individual portfolio paths | Red: median portfolio value",
    x = "Time step", y = "Portfolio value"
  ) +
  theme_minimal()
