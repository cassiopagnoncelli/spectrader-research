# devtools::load_all()

# ETL.
# Load quotes and VIX data.
#
# Output: Produces quotes with VIX added.
#
quotes <- qetl::get_sample_quotes()

macro <- fets::macro()
