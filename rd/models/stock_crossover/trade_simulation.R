devtools::load_all()

source("rd/models/stock_crossover/entry.R")
source("rd/models/stock_crossover/exit.R")

max_position_days <- 20

# Generate trading signals
signal_cutoff <- quantile(df_val$yhat, .999, na.rm = TRUE)
signal_cutoff
df_signals <- df_test %>%
  filter(yhat > signal_cutoff) %>%
  filter_signals(within_days = max_position_days) %>% # Discard nearby signals
  arrange(date)

# df_signals <- df_signals %>% filter(date < "2025-04-01")
# df_signals <- df_signals %>% filter(date > "2024-08-01", date < "2024-08-30")
df_signals

# Exits for each position
posl_raw <- position_cohort(df_signals, 5, max_position_days, metaX)
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
