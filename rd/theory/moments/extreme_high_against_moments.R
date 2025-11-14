cutoff <- .001

lm(y ~ y_skewness + y_kurtosis, df_test_yhats %>% filter(y > 1.4)) %>%
  { print(summary(.)) }

# Skewness
df_test_yhats %>% # Higher end
  filter(y_skewness > quantile(y_skewness, 1 - cutoff)) %>%
  pull(y) %>%
  cap_distribution(c(.01, .99)) %>%
  { print(analyse_distribution(., groups = mean(.))); . } %>%
  plot_distribution(vline = 1)

df_test_yhats %>% # Lower end
  filter(y_skewness < quantile(y_skewness, cutoff/2)) %>%
  pull(y) %>%
  cap_distribution(c(.01, .99)) %>%
  { print(analyse_distribution(., groups = mean(.))); . } %>%
  plot_distribution(vline = 1)

df_test_yhats %>% # All
  pull(y) %>%
  cap_distribution(c(.01, .99)) %>%
  { print(analyse_distribution(., groups = mean(.))); . } %>%
  plot_distribution(vline = 1)

# Kurtosis
df_test_yhats %>% # Higher end
  filter(y_kurtosis > quantile(y_kurtosis, 1 - cutoff)) %>%
  pull(y) %>%
  cap_distribution(c(.01, .99)) %>%
  { print(analyse_distribution(., groups = mean(.))); . } %>%
  plot_distribution(vline = 1)

df_test_yhats %>% # Lower end
  filter(y_kurtosis < quantile(y_kurtosis, cutoff)) %>%
  pull(y) %>%
  cap_distribution(c(.01, .99)) %>%
  { print(analyse_distribution(., groups = mean(.))); . } %>%
  plot_distribution(vline = 1)

df_test_yhats %>% # All
  pull(y) %>%
  cap_distribution(c(.01, .99)) %>%
  { print(analyse_distribution(., groups = mean(.))); . } %>%
  plot_distribution(vline = 1)
