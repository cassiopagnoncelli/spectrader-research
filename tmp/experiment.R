devtools::load_all()

db = DatasourcePostgres$new()

vix <- db$aggregates("VIX")
btc <- db$univariates("CBBTCUSD")

# lts: to be done...
