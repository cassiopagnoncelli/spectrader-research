y <- test_y
yhat <- ensemble_test_pred

target <- 0.22
df <- tibble(y, yhat)
df

df %>%
  filter(yhat > target) %>%
  ggplot(aes(x = y)) +
  geom_histogram(fill = "blue", alpha = 0.5)

df %>% summarise(
  n = n(),
  above = 100 * sum(y > target) / n(),
  below = 100 - above,
  min = min(y),
  q05 = quantile(y, 0.05),
  q25 = quantile(y, 0.25),
  q50 = quantile(y, 0.50),
  q75 = quantile(y, 0.75),
  q95 = quantile(y, 0.95),
  max = max(y),
  quantile = ifelse(
    min(y) < 0 && max(y) > 0,
    uniroot(function(p) quantile(y, p), c(0, 1))$root,
    NA_real_)
)

df %>% ggplot(aes(x = yhat, y = y)) +
  geom_point(alpha = 0.1) +
  geom_smooth(formula = y ~ x, method = "loess") +
  geom_hline(yintercept = target, color = "red", linetype = "dashed") +
  geom_vline(xintercept = target, color = "red", linetype = "dashed") +
  xlab("Predicted") +
  ylab("Actual") +
  ggtitle("Actual vs Predicted with Fine-Grain Adjustments")

# Precision Curve (ROC-style)
# Calculate precision at different thresholds
thresholds <- seq(min(yhat), max(yhat), length.out = 100)

precision_data <- map_dfr(thresholds, function(thresh) {
  predictions_above <- df %>% filter(yhat >= thresh)

  if (nrow(predictions_above) == 0) {
    return(tibble(threshold = thresh, precision = NA_real_, recall = 0))
  }

  precision <- sum(predictions_above$y > target) / nrow(predictions_above)
  recall <- sum(predictions_above$y > target) / sum(df$y > target)

  tibble(threshold = thresh, precision = precision, recall = recall)
})

# Precision vs Threshold plot
precision_filtered <- precision_data %>% filter(!is.na(precision))
min_precision <- min(precision_filtered$precision)
y_min <- min_precision * 0.5

precision_filtered %>%
  ggplot(aes(x = threshold, y = precision)) +
  geom_line(color = "blue", size = 1) +
  geom_vline(xintercept = target, color = "red", linetype = "dashed", alpha = 0.5) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(y_min, 1.0)) +
  xlab("Prediction Threshold") +
  ylab("Precision") +
  ggtitle("Precision Curve: Proportion of True Positives Above Threshold") +
  theme_minimal()
