get_fsg <- function(n = 5, allow_partial_results = FALSE, start_date = NULL, end_date = NULL) {
  db <- Fetl$new()
  db$fsg(n, allow_partial_results, start_date, end_date)
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
