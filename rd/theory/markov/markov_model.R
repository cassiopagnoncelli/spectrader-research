library(markovchain)
library(reshape2)
library(ggplot2)

se <- c(
  rep("bearish", 14),
  rep("bullish", 26),
  rep("neutral", 6),
  rep("bullish", 8),
  rep("neutral", 31),
  rep("bullish", 15),
  rep("bearish", 6),
  rep("neutral", 8),
  rep("bearish", 11),
  rep("bullish", 14),
  rep("bearish", 26),
  rep("bullish", 6),
  rep("neutral", 8),
  rep("bullish", 31),
  rep("neutral", 15),
  rep("bearish", 6),
  rep("bearish", 8),
  rep("neutral", 11)
)

mc_fit <- markovchainFit(data = se)
mc_model <- mc_fit$estimate

# Display the Markov Chain Model
mc_model
mc_model@states
mc_fit$standardError
mc_fit$upperEndpointMatrix
mc_fit$lowerEndpointMatrix

# Model State Diagram
plot(mc_model, main = "Markov Model State Diagram", edge.arrow.size = 0.5)

# Transition probability matrix plot
trans_mat <- mc_model@transitionMatrix
trans_df <- melt(trans_mat)
colnames(trans_df) <- c("From", "To", "Probability")
ggplot(trans_df, aes(x = To, y = From, fill = Probability)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.3f", Probability)), color = "black", size = 5) +
  scale_fill_gradient2(
    low = "white", high = "steelblue", mid = "lightblue",
    midpoint = 0.5, limit = c(0, 1)
  ) +
  labs(
    title = "Transition Probability Matrix",
    x = "To State",
    y = "From State"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

# Time proportion the system spends in each state
steadyStates(mc_model)

# how long (on average) it takes to transition from one state to another
meanFirstPassageTime(mc_model)

# Expected number of steps to return to a given state
meanRecurrenceTime(mc_model)

# Predict the next N states given a current state
# Simulate future sequences based on the learned transition probabilities
predict(mc_model, newdata = "bullish", n.ahead = 10)
rmarkovchain(n = 100, object = mc_model, t0 = "neutral")

# Can reach any state from any other?
is.irreducible(mc_model)

# Periodicity analysis
period(mc_model)

# Hitting probabilities
hittingProbabilities(mc_model)

# State entropy & predictability
steadyStates(mc_model) * log2(steadyStates(mc_model)) # Shannon entropy

# Condition probabilities
mc_model^2 # 2-step transitions
mc_model^10 # 10-step transitions

# Test if the model differs from a Random Walk
transition_counts <- table(se[-length(se)], se[-1]) # Actual count of probs
total_transitions <- sum(transition_counts) # Expected counts under random walk
expected_random <- matrix(total_transitions / (n_states^2),
  nrow = n_states, ncol = n_states
)

chi_sq <- sum((transition_counts - expected_random)^2 / expected_random)
df <- (n_states - 1)^2
p_value <- pchisq(chi_sq, df, lower.tail = FALSE)
p_value
