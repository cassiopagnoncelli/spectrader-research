devtools::load_all()

source("rd/models/stock_crossover/entry.R")
source("rd/models/stock_crossover/exit.R")

max_position_days <- 20

# Generate trading signals
df_signals <- df_test %>%
  filter(yhat > 1.37) %>%
  filter_signals(within_days = max_position_days) %>% # Discard nearby signals
  arrange(date)

# Exits for each position
posl_raw <- position_cohort(df_signals, 100, max_position_days, q)
posl <- lapply(seq_along(posl_raw), function(i) {
  exit_dqr(
    dqr_fits,
    max_position_days = max_position_days,
    side = "long"
  )(posl_raw[[i]])
})

# Signals & Returns
dfsr <- position_cohort_return(posl, df_signals)
df_dates <- prepare_df_dates(dfsr)

# Signal accuracy analysis
accuracy <- exit_accuracy(dfsr, side = "long")
accuracy_captured <- accuracy %>% filter(!is.na(exit_method))
accuracy_uncaptured <- accuracy %>% filter(is.na(exit_method))

# Dashboard
shiny::runApp("rd/models/stock_crossover/dashboard.R")
