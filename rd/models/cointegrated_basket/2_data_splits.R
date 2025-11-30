# Cointegration Analysis Experiment
# Transform quotes data and test for unit roots and cointegration
#
# Prerequisites: Run 1_etl.R first to load the 'q' tibble
# Expected format: q with columns (symbol, date, close)

library(tidyverse)
library(urca)      # for unit root and cointegration tests
library(knitr)     # for nice output formatting

industry_sel <- "Oil & Gas Drilling"

q <- quotes_raw %>%
  filter(industry == industry_sel) %>%
  filter(symbol %in% sample(unique(symbol), min(10, length(unique(symbol))))) %>%
  select(symbol, date, close)

q_wide <- q %>%
  pivot_wider(
    names_from = symbol,
    values_from = close,
    id_cols = date
  ) %>%
  arrange(date)

price_matrix <- q_wide %>%
  select(-date) %>%
  as.matrix()

test_cointegration_rank
