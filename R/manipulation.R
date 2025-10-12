get_fsg <- function(n = 5, allow_partial_results = FALSE, start_date = NULL, end_date = NULL) {
  db <- Fetl$new()
  db$fsg(n, allow_partial_results, start_date, end_date)
}

get_sfm <- function(from = '2020-01-01', to = today()) {
  db <- Fetl$new()
  db$stock_forward_mass(from, to)
}

get_quotes <- function(symbols, from = NULL, to = NULL, exchange = NULL, sector = NULL, industry = NULL, country = NULL) {
  db <- Fetl$new()
  db$quotes(symbols, from, to, exchange, sector, industry, country)
}

get_ticker <- function(ticker) {
  db <- Qetl$new()
  kind <- db$kind(ticker)
  if (kind == "aggregate") {
    db$aggregates(ticker)
  } else if (kind == "univariate") {
    db$univariates(ticker)
  } else {
    stop("Unknown ticker kind")
  }
}
