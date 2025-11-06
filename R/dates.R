add_business_days <- function(date, n) {
  # Adds n business days to the given date
  # date: Date object or vector of Date objects
  # n: integer or vector of integers, number of business days to add (can be negative)
  # Returns: Date object or vector of Date objects
  
  # Helper function for single date/n pair
  add_single <- function(single_date, single_n) {
    if (!inherits(single_date, "Date")) {
      stop("date must be a Date object")
    }
    
    if (!is.numeric(single_n) || length(single_n) != 1) {
      stop("n must be a single numeric value")
    }
    
    current_date <- single_date
    days_added <- 0
    
    while (days_added < abs(single_n)) {
      current_date <- current_date + sign(single_n)
      if (weekdays(current_date) %in% c("Saturday", "Sunday")) {
        next
      }
      days_added <- days_added + 1
    }
    
    current_date
  }
  
  # Vectorize the operation
  if (length(date) > 1 || length(n) > 1) {
    result <- mapply(add_single, date, n, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    result <- unlist(result)
    as.Date(result, origin = "1970-01-01")
  } else {
    add_single(date, n)
  }
}
