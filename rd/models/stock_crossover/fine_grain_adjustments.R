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
