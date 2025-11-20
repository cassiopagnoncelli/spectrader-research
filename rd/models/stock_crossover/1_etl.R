# devtools::load_all()

# ETL.
# Load quotes and VIX data.
#
# Output: Produces quotes with VIX added.
#
quotes <- qetl::get_sample_quotes()
macro <- fets::macro()
macro_cols <- colnames(macro)

# Left join with roll forward fill: keep all quotes, add macro data
# Roll join ensures missing macro dates use the last available value (forward fill)
quotes <- macro[quotes, on = "date", roll = TRUE]
