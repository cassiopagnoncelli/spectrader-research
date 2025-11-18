# devtools::load_all()

# ETL.
# Load quotes and VIX data.
#
# Output: Produces quotes with VIX added.
#
# TO DO: Bring further macro series, like UNRATE, CPI, etc.
#
vix <- fets::get_vix()
quotes <- fets::get_quotes()
fets::add_vix(quotes, vix)
