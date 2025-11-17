# devtools::load_all()

# ETL.
# Load quotes and VIX data.
#
# Output: Produces quotes with VIX added.
#
# TO DO: Bring further macro series, like UNRATE, CPI, etc.
#
fetl <- fets::Fetl$new()

vix <- fets::get_vix(fetl)
quotes <- fets::get_quotes(fetl)
fets::add_vix(quotes, vix)
