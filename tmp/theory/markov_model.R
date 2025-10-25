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
  scale_fill_gradient2(low = "white", high = "steelblue", mid = "lightblue",
                       midpoint = 0.5, limit = c(0, 1)) +
  labs(title = "Transition Probability Matrix",
       x = "To State",
       y = "From State") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

