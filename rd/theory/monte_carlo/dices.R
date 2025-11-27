set.seed(42)

n <- 1e6 # number of simulations

dice1 <- sample(1:6, n, replace = TRUE)
dice2 <- sample(1:6, n, replace = TRUE)

sum7 <- dice1 + dice2 == 7

# By law of large numbers, experiment converges to true probability
mean(sum7) # probability ≈ 0.1667
