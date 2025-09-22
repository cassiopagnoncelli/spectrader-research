fmr <- function(aggregates, ahead = 40, method = "mass") {
  if (!inherits(aggregates, "xts")) {
    stop("aggregates must be an xts object")
  }

  get_leads <- function(column) {
    function(n) dplyr::lead(aggregates[, column], n)
  }

  get_bars <- function(column) {
    do.call(cbind, lapply(seq_len(ahead), get_leads(column)))
  }

  high_leads <- log(sweep(get_bars("adjusted"), 1, aggregates[, "adjusted"], "/"))
  low_leads <- log(sweep(get_bars("adjusted"), 1, aggregates[, "adjusted"], "/"))
  leads <- merge(high_leads, low_leads)

  posleads <- apply(leads, 1, sum)
  posleads[is.na(posleads)] <- 0
  negleads <- apply(leads, 1, sum)
  negleads[is.na(negleads)] <- 0

  if (method == "mass") {
    calc <- posleads + negleads
    result <- cbind(posleads, negleads, calc)
    colnames(result) <- c("posleads", "negleads", "fmr")
    result <- xts::xts(result, order.by = zoo::index(aggregates))
    result
  } else {
    stop("method must be 'regularized' or 'ratio'")
  }
}
