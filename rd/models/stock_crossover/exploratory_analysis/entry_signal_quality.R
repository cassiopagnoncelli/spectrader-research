# Cutoffs for extreme identity predictions
y_high_hat_cutoff <- quantile(mnXYP[train_idx, ]$y_high_hat, .999)
y_low_hat_cutoff <- quantile(mnXYP[train_idx, ]$y_low_hat, .0001)

cat(sprintf("Cut-offs:\n  high_hat: %.4f\n   low_hat: %4f\n",
            y_high_hat_cutoff,
            y_low_hat_cutoff))

# Distribution
mnXYP[val_idx, ] %>%
  filter(y_high_hat > y_high_hat_cutoff, y_low_hat > y_low_hat_cutoff) %>%
  pull(extreme_high_identity) %>%
  { print(analyse_distribution(.)); . } %>%
  cap_distribution(c(.005, .995)) %>%
  plot_distribution(vline = 1)
