# devtools::load_all()

# ETL.
# Load quotes and VIX data.
#
# Output: Produces quotes with VIX added.
#
# TO DO: Bring further macro series, like UNRATE, CPI, etc.
#
quotes <- qetl::get_sample_quotes()
vix <- qetl::get_vix()
qetl::add_vix(quotes, vix)
