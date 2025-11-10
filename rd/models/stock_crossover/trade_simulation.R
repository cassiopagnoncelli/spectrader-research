devtools::load_all()

source("rd/models/stock_crossover/entry.R")
source("rd/models/stock_crossover/exit.R")

# Generate trading signals
df_signals <- df_test %>%
  filter(yhat > 1.4) %>%
  filter_signals(within_days = 20) %>% # Discard signals too close to each other
  arrange(date)

# Exits for each position
max_position_days <- 20
posl <- position_cohort(
  df_signals,
  before_days = 30,
  after_days = max_position_days,
  fun = exit_dqr(dqr_fits, max_position_days = max_position_days)
)

# Signals & Returns
dfsr <- position_cohort_return(posl, df_signals)
df_dates <- prepare_df_dates(dfsr)

# Signal accuracy analysis
accuracy <- exit_accuracy(dfsr, side = "long")
accuracy_captured <- accuracy %>% filter(!is.na(exit_method))
accuracy_uncaptured <- accuracy %>% filter(is.na(exit_method))

# Dashboard
shiny::runApp("rd/models/stock_crossover/dashboard.R")
