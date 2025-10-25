s <- c(
  pmax(rnorm(100, 0.2, 0.05), 0),
  pmin(rnorm(100, 0.6, 0.09), 1)
)

# MASS::truehist(set)

# Density estimator.
kd1 <- density(s, from = 0, to = 1)
kd2 <- density(s, from = 0, to = 1, kernel = "epanechnikov")
plot(kd1)
plot(kd2)

kd <- kd1

# CDF estimator.
cdf <- function(q) {
  approx(kd$x, cumsum(kd$y) / sum(kd$y), xout = q)$y
}

cdf(0.01)
cdf(0.4)
cdf(0.6)
cdf(0.8)
cdf(0.99)
