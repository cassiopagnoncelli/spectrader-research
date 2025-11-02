plot_distribution <- function(data, bins = NULL, vline = 0, title = "Distribution") {
  # Handle different input types
  if (is.vector(data) || is.atomic(data)) {
    # If data is a vector, convert to tibble
    data <- tibble::tibble(value = data)
  } else if (is.data.frame(data)) {
    # If data is a tibble/data.frame, find first numeric column
    numeric_cols <- sapply(data, is.numeric)
    if (!any(numeric_cols)) {
      stop("No numeric columns found in the data")
    }
    first_numeric <- names(data)[which(numeric_cols)[1]]
    data <- tibble::tibble(value = data[[first_numeric]])
  }

  # Calculate number of bins using Sturges' rule if not specified
  if (is.null(bins)) {
    n <- nrow(data)
    bins <- ceiling(3.3 * log10(n) + 1)
  }

  # Create histogram with density scale and smoothing curve
  ggplot(data, aes(x = value)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = bins,
                   fill = "pink",
                   color = "pink",
                   alpha = 0.4) +
    geom_density(aes(y = after_stat(density)),
                 fill = "lightblue",
                 alpha = 0.7,
                 color = "lightblue",
                 linewidth = .2) +
    geom_vline(xintercept = vline,
               color = "grey70",
               linetype = "solid",
               linewidth = .4) +
    labs(title = title,
         x = "Value",
         y = "Density") +
    theme_minimal() +
    theme(panel.grid = element_blank())
}

analyse_distribution <- function(data, groups = c(0)) {
  # Handle different input types
  if (is.vector(data) || is.atomic(data)) {
    # If data is a vector, convert to tibble
    data <- tibble::tibble(value = data)
  } else if (is.data.frame(data)) {
    # If data is a tibble/data.frame, find first numeric column
    numeric_cols <- sapply(data, is.numeric)
    if (!any(numeric_cols)) {
      stop("No numeric columns found in the data")
    }
    first_numeric <- names(data)[which(numeric_cols)[1]]
    data <- tibble::tibble(value = data[[first_numeric]])
  }

  # Sort groups to ensure proper ordering
  groups <- sort(groups)

  # Create group labels and split data
  n_groups <- length(groups) + 1
  group_labels <- paste0("g", 1:n_groups)

  # Assign each value to a group
  data <- data |>
    dplyr::mutate(
      group = dplyr::case_when(
        value < groups[1] ~ group_labels[1],
        TRUE ~ group_labels[n_groups]
      )
    )

  # Handle intermediate groups if there are multiple thresholds
  if (length(groups) > 1) {
    for (i in 2:length(groups)) {
      data <- data |>
        dplyr::mutate(
          group = dplyr::if_else(
            value >= groups[i - 1] & value < groups[i],
            group_labels[i],
            group
          )
        )
    }
  }

  # Calculate statistics for each group
  quantile_probs <- c(0, 0.01, 0.05, 0.32, 0.5, 0.68, 0.95, 0.99, 1)

  # Get total dataset size
  total_n <- nrow(data)

  group_results <- data |>
    dplyr::group_by(group) |>
    dplyr::summarise(
      count = dplyr::n(),
      prob = dplyr::n() / total_n,
      mean = mean(value, na.rm = TRUE),
      expected = prob * mean,
      sd = sd(value, na.rm = TRUE),
      min = quantile(value, probs = 0, na.rm = TRUE),
      q_0.01 = quantile(value, probs = 0.01, na.rm = TRUE),
      q_0.05 = quantile(value, probs = 0.05, na.rm = TRUE),
      q_0.32 = quantile(value, probs = 0.32, na.rm = TRUE),
      q_0.50 = quantile(value, probs = 0.5, na.rm = TRUE),
      q_0.68 = quantile(value, probs = 0.68, na.rm = TRUE),
      q_0.95 = quantile(value, probs = 0.95, na.rm = TRUE),
      q_0.99 = quantile(value, probs = 0.99, na.rm = TRUE),
      max = quantile(value, probs = 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(group = factor(group, levels = group_labels)) |>
    dplyr::arrange(group)

  # Calculate dataset metrics
  overall_results <- data |>
    dplyr::summarise(
      expected_value = group_results$expected |> sum(na.rm = TRUE),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      n = nrow(data)
    )

  return(list(
    overall_results = overall_results,
    group_results = group_results
  ))
}
