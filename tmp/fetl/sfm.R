devtools::load_all()

# Get Stock Forward Mass
sfm <- get_sfm(from = "2021-01-01") %>% tibble
sfm

# Plot summary of upside and downside columns with histograms
sfm_summary <- sfm %>%
  dplyr::select(upside, downside) %>%
  pivot_longer(cols = everything(), names_to = "metric", values_to = "value") %>%
  dplyr::filter(value >= -30 & value <= 30)

ggplot(sfm_summary, aes(x = value, fill = metric)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  facet_wrap(~metric, scales = "free_y") +
  labs(
    title = "Summary of Upside and Downside",
    x = "Value",
    y = "Count"
  ) +
  theme_minimal()
