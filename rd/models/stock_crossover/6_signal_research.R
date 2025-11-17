if (FALSE) {
  # devtools::load_all()

  source("rd/models/stock_crossover/1_etl.R")
  source("rd/models/stock_crossover/2_feature_engineering.R")
  source("rd/models/stock_crossover/3_splits.R")
  source("rd/models/stock_crossover/4_feature_enrichment.R")
  source("rd/models/stock_crossover/5_datasets.R")
  source("rd/models/stock_crossover/6_signal_models.R")
}

# SIGNAL RESEARCH.
# Research on entry signals.
#
# Output: Intermediate step for signal models.

mnXYP[test_idx, ] %>%
  summarise(
    cor_ehi = cor(extreme_high_identity, yhat_ehi),
    cor_eli = cor(extreme_high_identity, yhat_eli)
  )

mnXYP[test_idx, ] %>%
  filter(
    yhat_ehi > quantile(yhat_ehi, ehi_cutoff),
    yhat_eli > quantile(yhat_eli, eli_cutoff)
  ) %>%
  filter_signals(within_days = 20) %>%
  mutate(y = extreme_high_identity - 1) %>%
  pull(y) %>%
  analyse_distribution(groups = c(.09))
