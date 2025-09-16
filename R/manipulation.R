get_ticker <- function(ticker) {
  db <- DatasourcePostgres$new()
  kind <- db$kind(ticker)
  if (kind == "aggregate") {
    return(db$aggregates(ticker))
  } else if (kind == "univariate") {
    return(db$univariates(ticker))
  } else {
    stop("Unknown ticker kind")
  }
}

align <- function(..., fill = NA, locf = TRUE, timeframe = NULL, names = TRUE) {
  # Get the xts objects and their names
  xts_objects <- list(...)
  
  if (names) {
    # Use the original variable names from the calling environment
    call_names <- as.character(substitute(list(...)))[-1]
    xts_names <- call_names
  } else {
    # Use provided names or create default names
    xts_names <- names(xts_objects)
    if (is.null(xts_names)) {
      xts_names <- paste0("series", seq_along(xts_objects))
    }
  }
  
  # Validate inputs
  if (length(xts_objects) == 0) {
    stop("At least one xts object is required for alignment")
  }
  
  # Check that all objects are xts
  if (!all(sapply(xts_objects, function(x) inherits(x, "xts")))) {
    stop("All objects must be xts time series")
  }
  
  # Rename columns in each xts object to include the series name
  for (i in seq_along(xts_objects)) {
    series_name <- xts_names[i]
    if (is.null(series_name) || series_name == "") {
      series_name <- paste0("series", i)
    }
    
    # Get column names and prepend series name
    col_names <- colnames(xts_objects[[i]])
    if (is.null(col_names)) {
      col_names <- "value"
    }
    new_col_names <- paste(series_name, col_names, sep = ".")
    colnames(xts_objects[[i]]) <- new_col_names
  }
  
  # Merge all xts objects using full join (all = TRUE)
  if (length(xts_objects) == 1) {
    aligned_xts <- xts_objects[[1]]
  } else {
    aligned_xts <- Reduce(function(x, y) merge(x, y, all = TRUE), xts_objects)
  }
  
  # Handle missing values
  if (locf && !is.na(fill)) {
    # First fill with specified value, then apply locf
    aligned_xts[is.na(aligned_xts)] <- fill
    aligned_xts <- zoo::na.locf(aligned_xts, na.rm = FALSE)
  } else if (locf) {
    # Apply locf only
    aligned_xts <- zoo::na.locf(aligned_xts, na.rm = FALSE)
  } else if (!is.na(fill)) {
    # Fill with specified value only
    aligned_xts[is.na(aligned_xts)] <- fill
  }
  
  return(aligned_xts)
}
