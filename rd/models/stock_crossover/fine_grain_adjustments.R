y <- test_y
yhat <- ensemble_test_pred

target <- 0.01
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

df %>%
  filter(yhat > target) %>%
  arrange(desc(yhat)) %>%
  mutate(
    rank = row_number(),
    cum_above = cumsum(y > target),
    precision = cum_above / rank
  ) %>%
  ggplot(aes(x = rank, y = precision)) +
  geom_line() +
  geom_hline(yintercept = 100 * sum(df$y > target) / nrow(df), linetype = "dashed", color = "red") +
  labs(
    title = "Precision at Different Ranks",
    x = "Rank",
    y = "Precision (%)"
  )
