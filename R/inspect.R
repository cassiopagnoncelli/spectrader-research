inspect <- function(X) {
  suppressWarnings({
    result <- purrr::map_df(names(X), function(var_name) {
      col_data <- X[[var_name]]
      
      # Basic statistics
      n_total <- length(col_data)
      n_valid <- sum(!is.na(col_data))
      n_missing <- sum(is.na(col_data))
      pct_missing <- (n_missing / n_total) * 100
      
      # Type detection
      var_type <- class(col_data)[1]
      
      # Initialize result tibble
      result <- tibble::tibble(
        variable = var_name,
        type = var_type,
        n_total = n_total,
        n_valid = n_valid,
        n_missing = n_missing,
        pct_missing = round(pct_missing, 2)
      )
      
      # Numeric/integer statistics
      if (is.numeric(col_data) || is.integer(col_data)) {
        valid_data <- col_data[!is.na(col_data)]
        
        if (length(valid_data) > 0) {
          result <- result %>%
            dplyr::mutate(
              mean = mean(valid_data, na.rm = TRUE),
              median = median(valid_data, na.rm = TRUE),
              sd = sd(valid_data, na.rm = TRUE),
              min = min(valid_data, na.rm = TRUE),
              max = max(valid_data, na.rm = TRUE),
              q25 = quantile(valid_data, 0.25, na.rm = TRUE),
              q75 = quantile(valid_data, 0.75, na.rm = TRUE),
              iqr = IQR(valid_data, na.rm = TRUE),
              range = max(valid_data, na.rm = TRUE) - min(valid_data, na.rm = TRUE),
              cv = ifelse(mean(valid_data, na.rm = TRUE) != 0, 
                        sd(valid_data, na.rm = TRUE) / abs(mean(valid_data, na.rm = TRUE)), 
                        NA),
              n_unique = length(unique(valid_data)),
              n_zeros = sum(valid_data == 0, na.rm = TRUE),
              pct_zeros = round((sum(valid_data == 0, na.rm = TRUE) / length(valid_data)) * 100, 2)
            )
        } else {
          result <- result %>%
            dplyr::mutate(
              mean = NA_real_, median = NA_real_, sd = NA_real_,
              min = NA_real_, max = NA_real_, q25 = NA_real_, 
              q75 = NA_real_, iqr = NA_real_, range = NA_real_,
              cv = NA_real_, n_unique = NA_integer_, 
              n_zeros = NA_integer_, pct_zeros = NA_real_
            )
        }
      } else if (is.character(col_data) || is.factor(col_data)) {
        # Categorical statistics
        valid_data <- col_data[!is.na(col_data)]
        
        if (length(valid_data) > 0) {
          result <- result %>%
            dplyr::mutate(
              n_unique = length(unique(valid_data)),
              most_frequent = names(sort(table(valid_data), decreasing = TRUE))[1],
              most_frequent_count = max(table(valid_data)),
              mean = NA_real_, median = NA_real_, sd = NA_real_,
              min = NA_real_, max = NA_real_, q25 = NA_real_, 
              q75 = NA_real_, iqr = NA_real_, range = NA_real_,
              cv = NA_real_, n_zeros = NA_integer_, pct_zeros = NA_real_
            )
        } else {
          result <- result %>%
            dplyr::mutate(
              n_unique = NA_integer_, most_frequent = NA_character_,
              most_frequent_count = NA_integer_,
              mean = NA_real_, median = NA_real_, sd = NA_real_,
              min = NA_real_, max = NA_real_, q25 = NA_real_, 
              q75 = NA_real_, iqr = NA_real_, range = NA_real_,
              cv = NA_real_, n_zeros = NA_integer_, pct_zeros = NA_real_
            )
        }
      } else {
        # Other types - minimal statistics
        result <- result %>%
          dplyr::mutate(
            n_unique = length(unique(col_data[!is.na(col_data)])),
            mean = NA_real_, median = NA_real_, sd = NA_real_,
            min = NA_real_, max = NA_real_, q25 = NA_real_, 
            q75 = NA_real_, iqr = NA_real_, range = NA_real_,
            cv = NA_real_, n_zeros = NA_integer_, pct_zeros = NA_real_
          )
      }
      
      return(result)
    })
    
    # Sort by variable name
    result <- result %>%
      dplyr::arrange(variable)
  })
}
