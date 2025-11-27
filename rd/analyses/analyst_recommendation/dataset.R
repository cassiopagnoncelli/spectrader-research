devtools::load_all()
library("caret")

options(scipen = 999)

fetl <- Fetl$new()

# ~5-min query.
fwd_raw <- fetl$send_query(sprintf("
    SELECT
      analyst_company,
      published_date::DATE,
      ROUND(price_target / price_when_posted - 1, 3) AS yhat,
      ROUND(fwd('%s', c.symbol, published_date::DATE, %d) - 1, 3) AS y
    FROM price_target_news ptn
    JOIN companies c ON ptn.company_id = c.id
    WHERE ptn.published_date <= '2025-01-01'
      AND price_target > 0
      AND price_when_posted > 0
    LIMIT %d
  ", "mean_identity", 90, 55000)) %>%
  tibble()

fwd_raw %>%
  na.omit() %>%
  group_by(analyst_company) %>%
  summarise(
    n = n(),
    cor = cor(yhat, y),
    r.squared = caret::R2(yhat, y)
  ) %>%
  filter(n >= 30) %>%
  arrange(desc(abs(cor))) %>%
  print(n = 100) %>%
  ggplot(aes(x = reorder(analyst_company, cor), y = cor)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Correlation between Analyst Price Target Predictions and Actual 90-Day Forward Returns",
    x = "Analyst Company",
    y = "Correlation Coefficient"
  ) +
  theme_minimal()

fwd_raw %>%
  filter(analyst_company == "Benchmark Co.") %>%
  mutate(yhat = 100 * yhat, y = 100 * y) %>%
  print(n = 25) %>%
  ggplot(aes(x = yhat, y = y)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(
    title = "Benchmark Co. Analyst Price Target Predictions vs. Actual 90-Day Forward Returns",
    x = "Predicted 90-Day Forward Return (%)",
    y = "Actual 90-Day Forward Return (%)"
  ) +
  theme_minimal()
