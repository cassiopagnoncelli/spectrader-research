# Brownian motion (aka Wiener process)
n <- 10000
dt <- 1/n
W <- cumsum(rnorm(n, mean = 0, sd = sqrt(dt)))

plot(W, type = "l")

# Monte Carlo simulation
T = 1
n_steps = 252
n_paths = 10000

dt  <- T / n_steps

# matrix of N(0, dt) increments: rows = paths, cols = steps
dW  <- matrix(rnorm(n_paths * n_steps, mean = 0, sd = sqrt(dt)),
              nrow = n_paths, ncol = n_steps)

# cumulative sum across columns to build W_t
# apply(..., 1, cumsum) would be slow for large n_paths, so we do this trick:
W <- t(apply(dW, 1, cumsum))
W <- cbind(rep(0, n_paths), W) # prepend W0 = 0 at column 1
plot(W[1, ], type = "l")
plot(W[2, ], type = "l")

# time grid
t <- seq(0, T, length.out = n_steps + 1)

# Variance should grow linearly with time
plot(apply(W, 2, var), t='l')
