library("ggplot2")
library("dplyr")

# Logit function
x <- seq(0.01, 0.99, by = 0.01)
logit <- log(x / (1 - x))

ggplot(data.frame(x, logit), aes(x = x, y = logit)) +
  geom_line(color = "blue") +
  labs(title = "Logit Function",
       x = "Probability",
       y = "Logit") +
  theme_minimal() +
  geom_hline(yintercept = c(-1, 1), color = "red", linetype = "dashed")

# Inverse Logit and Related Functions
y <- x / (1 - x)
loginv <- log((1 + x) / (1 - x))
invhyptan <- sqrt((1 + x) / (1 - x)) - 1
df <- data.frame(x, y, invhyptan, loginv)

subtitles = c(
  "Odds Ratio: x/(1-x)",
  "Inverse Hyperbolic Tangent: log(1+x) - log(1-x)",
  "Logit Inverse: sqrt((1+x)/(1-x))"
)
ggplot(df %>% filter(x <= 0.8), aes(x = x)) +
  geom_line(aes(y = y, color = subtitles[1])) +
  geom_line(aes(y = invhyptan, color = subtitles[2])) +
  geom_line(aes(y = loginv, color = subtitles[3])) +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +    # horizontal line at y=1
  geom_vline(xintercept = 0.5, color = "red", linetype = "dashed") +  # vertical line at x=0.5 (fixed)
  labs(
    title = "Logit and Related Functions",
    x = "Probability",
    y = "Value"
  ) +
  scale_color_manual(
    values = c(
      "Odds Ratio: x/(1-x)" = "blue",
      "Inverse Hyperbolic Tangent: log(1+x) - log(1-x)" = "green",
      "Logit Inverse: sqrt((1+x)/(1-x))" = "orange"
    )
  ) +
  theme_minimal()
