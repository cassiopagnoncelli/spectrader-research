df <- tibble(
  symbol = fwd_metadata$symbol[test_indices],
  date = fwd_metadata$date[test_indices],
  y = results$actuals$test,
  yhat = results$predictions$test,
  close_id = fwd$y_7[test_indices]
)

df_signals <- df %>%
  filter(yhat > 1.31)
df_signals %>% print(n = 50)

df_signals %>%
  { print(skimr::skim(.)); . } %>%
  ggplot(aes(x = close_id)) +
  geom_histogram(
    aes(y = ..density..),
    bins = 30,
    fill = "lightblue",
    color = "white",
    alpha = 0.6
  ) +
  geom_density(
    color = "#2C3E50",
    size = 1.2,
    alpha = 0.9
  ) +
  labs(
    title = "Distribution of Actual Returns for High Predicted Returns",
    x = "Actual Return",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.background = element_rect(fill = "#f8f9fa", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "#eaeaea"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  )
