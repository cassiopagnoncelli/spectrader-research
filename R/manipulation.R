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

align <- function(..., fill = NA, locf = TRUE, names = TRUE, timeframe = NULL, aggregates = c("open", "high", "low", "close")) {
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
  
  # Handle timeframe aggregation
  if (!is.null(timeframe)) {
    # Define timeframe mappings
    timeframe_map <- list(
      "M1" = "mins",
      "H1" = "hours", 
      "D1" = "days",
      "W1" = "weeks",
      "MN1" = "months",
      "Q" = "quarters",
      "Y" = "years"
    )
    
    if (!timeframe %in% names(timeframe_map)) {
      stop("Invalid timeframe. Must be one of: M1, H1, D1, W1, MN1, Q, Y")
    }
    
    period_func <- timeframe_map[[timeframe]]
    
    # Get original column names
    original_cols <- colnames(aligned_xts)
    
    # Create aggregated columns
    aggregated_list <- list()
    
    for (col in original_cols) {
      for (agg in aggregates) {
        new_col_name <- paste(col, agg, sep = ".")
        
        if (agg == "open") {
          aggregated_list[[new_col_name]] <- xts::period.apply(aligned_xts[, col], 
                                                              xts::endpoints(aligned_xts, period_func), 
                                                              function(x) if(length(x) > 0 && !all(is.na(x))) x[1] else NA)
        } else if (agg == "high") {
          aggregated_list[[new_col_name]] <- xts::period.apply(aligned_xts[, col], 
                                                              xts::endpoints(aligned_xts, period_func), 
                                                              function(x) if(length(x) > 0 && !all(is.na(x))) max(x, na.rm = TRUE) else NA)
        } else if (agg == "low") {
          aggregated_list[[new_col_name]] <- xts::period.apply(aligned_xts[, col], 
                                                              xts::endpoints(aligned_xts, period_func), 
                                                              function(x) if(length(x) > 0 && !all(is.na(x))) min(x, na.rm = TRUE) else NA)
        } else if (agg == "close") {
          aggregated_list[[new_col_name]] <- xts::period.apply(aligned_xts[, col], 
                                                              xts::endpoints(aligned_xts, period_func), 
                                                              function(x) if(length(x) > 0 && !all(is.na(x))) x[length(x)] else NA)
        } else if (agg == "mode") {
          aggregated_list[[new_col_name]] <- xts::period.apply(aligned_xts[, col], 
                                                              xts::endpoints(aligned_xts, period_func), 
                                                              function(x) {
                                                                if(length(x) > 0 && !all(is.na(x))) {
                                                                  x_clean <- x[!is.na(x)]
                                                                  if(length(x_clean) > 0) {
                                                                    tbl <- table(x_clean)
                                                                    as.numeric(names(tbl)[which.max(tbl)])
                                                                  } else NA
                                                                } else NA
                                                              })
        } else if (agg == "average") {
          aggregated_list[[new_col_name]] <- xts::period.apply(aligned_xts[, col], 
                                                              xts::endpoints(aligned_xts, period_func), 
                                                              function(x) if(length(x) > 0 && !all(is.na(x))) mean(x, na.rm = TRUE) else NA)
        } else if (agg == "typical") {
          aggregated_list[[new_col_name]] <- xts::period.apply(aligned_xts[, col], 
                                                              xts::endpoints(aligned_xts, period_func), 
                                                              function(x) {
                                                                if(length(x) > 0 && !all(is.na(x))) {
                                                                  x_clean <- x[!is.na(x)]
                                                                  if(length(x_clean) > 0) {
                                                                    low_val <- min(x_clean)
                                                                    high_val <- max(x_clean)
                                                                    low_val + (high_val - low_val) / 3
                                                                  } else NA
                                                                } else NA
                                                              })
        }
      }
    }
    
    # Combine all aggregated columns into a single xts object
    if (length(aggregated_list) > 0) {
      # Ensure proper column names are preserved
      for (i in seq_along(aggregated_list)) {
        colnames(aggregated_list[[i]]) <- names(aggregated_list)[i]
      }
      aligned_xts <- do.call(merge, aggregated_list)
    }
  }
  
  return(aligned_xts)
}
