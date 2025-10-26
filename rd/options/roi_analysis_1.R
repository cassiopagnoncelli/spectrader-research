# Black-Scholes Put Option Pricing
bs_put <- function(S, K, r = 0.05, T = 63/252, sigma = 0.2) {
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  put_price <- K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
  return(put_price)
}

# Simulate log-normal terminal stock prices
simulate_lognormal_prices <- function(S0, mean_ST, n_sims = 10000, T = 63/252) {
  # Calculate parameters for log-normal distribution
  # E[S_T] = S0 * exp(mu * T) = mean_ST
  # So mu = log(mean_ST / S0) / T
  mu <- log(mean_ST / S0) / T

  # Assume a reasonable volatility
  sigma <- 0.2

  # Simulate prices: S_T = S0 * exp((mu - 0.5*sigma^2)*T + sigma*sqrt(T)*Z)
  Z <- rnorm(n_sims)
  ST <- S0 * exp((mu - 0.5 * sigma^2) * T + sigma * sqrt(T) * Z)

  return(list(
    prices = ST,
    mu = mu,
    sigma = sigma,
    mean_realized = mean(ST),
    mean_target = mean_ST
  ))
}

# Calculate put payoff at expiry
put_payoff <- function(ST, K) {
  return(pmax(K - ST, 0))
}

# Calculate ROI for a strategy
calculate_roi <- function(initial_cost, final_payoff) {
  roi <- (final_payoff - initial_cost) / initial_cost * 100
  return(roi)
}

# Strategy 1: Long Put at strike K
strategy_long_put <- function(S0, ST_sims, K, r = 0.05, T = 63/252, sigma = 0.2) {
  # Initial cost: buy put
  initial_cost <- bs_put(S0, K, r, T, sigma)

  # Final payoff
  payoffs <- put_payoff(ST_sims, K)

  # ROI for each simulation
  roi <- calculate_roi(initial_cost, payoffs)

  return(list(
    strategy = paste0("Long Put K=", K),
    initial_cost = initial_cost,
    mean_payoff = mean(payoffs),
    mean_roi = mean(roi),
    median_roi = median(roi),
    sd_roi = sd(roi),
    prob_profit = mean(roi > 0),
    max_loss = min(roi),
    max_gain = max(roi)
  ))
}

# Strategy 2: Put Spread (Buy high strike, sell low strike)
strategy_put_spread <- function(S0, ST_sims, K_buy, K_sell, r = 0.05, T = 63/252, sigma = 0.2) {
  # Initial cost: buy put at K_buy, sell put at K_sell
  cost_buy <- bs_put(S0, K_buy, r, T, sigma)
  credit_sell <- bs_put(S0, K_sell, r, T, sigma)
  initial_cost <- cost_buy - credit_sell

  # Final payoff
  payoffs_buy <- put_payoff(ST_sims, K_buy)
  payoffs_sell <- -put_payoff(ST_sims, K_sell)
  payoffs <- payoffs_buy + payoffs_sell

  # ROI for each simulation
  roi <- calculate_roi(initial_cost, payoffs)

  return(list(
    strategy = paste0("Put Spread ", K_buy, "/", K_sell),
    initial_cost = initial_cost,
    mean_payoff = mean(payoffs),
    mean_roi = mean(roi),
    median_roi = median(roi),
    sd_roi = sd(roi),
    prob_profit = mean(roi > 0),
    max_loss = min(roi),
    max_gain = max(roi)
  ))
}

# Strategy 3: No Hedge (baseline comparison)
strategy_no_hedge <- function(S0, ST_sims) {
  # "Initial cost" is holding stock - use S0 as reference
  initial_cost <- S0

  # Final "payoff" is just stock value
  payoffs <- ST_sims

  # ROI
  roi <- calculate_roi(initial_cost, payoffs)

  return(list(
    strategy = "No Hedge (Stock Only)",
    initial_cost = initial_cost,
    mean_payoff = mean(payoffs),
    mean_roi = mean(roi),
    median_roi = median(roi),
    sd_roi = sd(roi),
    prob_profit = mean(roi > 0),
    max_loss = min(roi),
    max_gain = max(roi)
  ))
}

# Main simulation function
run_simulation <- function(S0 = 100, mean_ST = 130, K = 130, n_sims = 10000, seed = 42) {
  set.seed(seed)

  # Parameters
  r <- 0.05
  T <- 63/252  # ~3 months
  sigma <- 0.2

  cat("=== PUT OPTION STRATEGY SIMULATION ===\n")
  cat("Initial Stock Price (S0):", S0, "\n")
  cat("Expected Terminal Price (mean_ST):", mean_ST, "\n")
  cat("Reference Strike (K):", K, "\n")
  cat("Number of Simulations:", n_sims, "\n")
  cat("Time to Expiry (T):", T, "years (~", round(T*252), "days)\n")
  cat("Risk-free Rate (r):", r, "\n")
  cat("Volatility (sigma):", sigma, "\n\n")

  # Simulate terminal prices
  cat("Simulating log-normal terminal prices...\n")
  sim_data <- simulate_lognormal_prices(S0, mean_ST, n_sims, T)
  ST_sims <- sim_data$prices
  cat("Mean simulated price:", round(sim_data$mean_realized, 2),
      "(target:", sim_data$mean_target, ")\n\n")

  # Test different strategies
  strategies <- list()

  # No hedge baseline
  strategies[[1]] <- strategy_no_hedge(S0, ST_sims)

  # Long puts at different strikes
  strategies[[2]] <- strategy_long_put(S0, ST_sims, K = 110, r, T, sigma)
  strategies[[3]] <- strategy_long_put(S0, ST_sims, K = 120, r, T, sigma)
  strategies[[4]] <- strategy_long_put(S0, ST_sims, K = 130, r, T, sigma)
  strategies[[5]] <- strategy_long_put(S0, ST_sims, K = 140, r, T, sigma)

  # Put spreads
  strategies[[6]] <- strategy_put_spread(S0, ST_sims, K_buy = 130, K_sell = 120, r, T, sigma)
  strategies[[7]] <- strategy_put_spread(S0, ST_sims, K_buy = 140, K_sell = 130, r, T, sigma)
  strategies[[8]] <- strategy_put_spread(S0, ST_sims, K_buy = 130, K_sell = 110, r, T, sigma)

  # Convert to data frame for easy comparison
  results_df <- do.call(rbind, lapply(strategies, function(s) {
    data.frame(
      Strategy = s$strategy,
      Initial_Cost = round(s$initial_cost, 2),
      Mean_Payoff = round(s$mean_payoff, 2),
      Mean_ROI = round(s$mean_roi, 2),
      Median_ROI = round(s$median_roi, 2),
      SD_ROI = round(s$sd_roi, 2),
      Prob_Profit = round(s$prob_profit * 100, 1),
      Max_Loss = round(s$max_loss, 2),
      Max_Gain = round(s$max_gain, 2)
    )
  }))

  # Sort by mean ROI
  results_df <- results_df[order(-results_df$Mean_ROI), ]
  rownames(results_df) <- NULL

  cat("=== RESULTS (sorted by Mean ROI) ===\n\n")
  print(results_df)

  cat("\n=== KEY INSIGHTS ===\n")
  best_strategy <- results_df[1, ]
  cat("Best Mean ROI:", best_strategy$Strategy, "with", best_strategy$Mean_ROI, "%\n")
  cat("Best Prob. of Profit:",
      results_df[which.max(results_df$Prob_Profit), "Strategy"],
      "with", max(results_df$Prob_Profit), "%\n")

  # Return detailed results
  invisible(list(
    simulated_prices = ST_sims,
    results = results_df,
    strategies = strategies
  ))
}

# Run the simulation
results <- run_simulation(S0 = 100, mean_ST = 130, K = 130, n_sims = 10000)
