if (FALSE) {
  devtools::load_all()
}

# ETL.
# Load quotes and VIX data.
#
# Output: Produces quotes with VIX added.
#
quotes <- qetl::get_sample_quotes()
quotes <- quotes[symbol %in% sample(unique(symbol), 10)]

macro <- fets::macro()

sentiments <- qetl::sentiments()
