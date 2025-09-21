fmr <- function(aggregates, ahead = 40, method = "regularized") {
  if (!inherits(aggregates, "xts")) {
    stop("aggregates must be an xts object")
  }

  get_leads <- function(column) {
    function(n) dplyr::lead(aggregates[, column], n)
  }

  get_bars <- function(column) {
    do.call(cbind, lapply(seq_len(ahead), get_leads(column)))
  }

  high_leads <- get_bars("high") / as.vector(aggregates[, "close"]) - 1
  high_leads[is.na(high_leads)] <- 0
  low_leads <- get_bars("low") / as.vector(aggregates[, "close"]) - 1
  low_leads[is.na(low_leads)] <- 0
  leads <- merge(high_leads, low_leads)

  posleads <- apply(pmax(leads, 0), 1, sum)
  negleads <- abs(apply(pmin(leads, 0), 1, sum))

  if (method == "regularized") {
    calc <- sqrt((1 + posleads) / (1 + negleads))
    result <- cbind(posleads, negleads, calc)
    colnames(result) <- c("posleads", "negleads", "fmr")
    result <- xts::xts(result, order.by = index(aggregates))
    result
  } else if (method == "ratio") {
    calc <- (posleads + negleads) / (1 + negleads)
    result <- cbind(posleads, negleads, calc)
    colnames(result) <- c("posleads", "negleads", "fmr")
    result <- xts::xts(result, order.by = index(aggregates))
    result
  } else {
    stop("method must be 'regularized' or 'ratio'")
  }
}
