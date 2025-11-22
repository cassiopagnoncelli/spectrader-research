if (FALSE) {
  devtools::load_all()

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
    cor_ehi = cor(qeh, yhat_qeh),
    cor_eli = cor(qel, yhat_qel)
  )

mnXYP[test_idx, ] %>%
  filter(
    yhat_qeh > quantile(yhat_qeh, qeh_cutoff),
    yhat_qel > quantile(yhat_qel, qel_cutoff)
  ) %>%
  filter_signals(within_days = 20) %>%
  mutate(y = excursion_high - 1) %>%
  pull(y) %>%
  analyse_distribution(groups = c(.09))
