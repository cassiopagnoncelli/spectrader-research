a <- cumsum(rnorm(1000, 0.2, 0.15))
b <- 100 + 10*cumsum(rnorm(1000, 1, 2))^2 + cumsum(rnorm(1000, -1, 1))

mat <- as.matrix(data.frame(a, b))

trace <- ca.jo(mat, type = "trace", ecdet = "const", K = 2)

summary(trace)

# plot(trace)


linearcomb <- a - 2.575441e-05 * b

require("MASS")
truehist(linearcomb)
