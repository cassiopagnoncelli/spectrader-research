df <- tibble(
  y = results$actuals$test,
  yhat = results$predictions$test
)

df %>%
  filter(yhat > 1.7) %>%
  { print(skimr::skim(.)); . } %>%
  ggplot(aes(x = y)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Actual Returns for High Predicted Returns",
    x = "Actual Return",
    y = "Frequency"
  ) +
  theme_minimal()
