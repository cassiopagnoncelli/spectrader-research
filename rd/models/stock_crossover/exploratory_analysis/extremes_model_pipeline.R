suppressPackageStartupMessages({
  library(data.table)
  library(VGAM)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(tibble)
})

# ------------------------------------------------------------------------------
# Helper builders ----------------------------------------------------------------
# ------------------------------------------------------------------------------

build_base_df <- function(indices) {
  if (is.null(indices) || length(indices) == 0) return(NULL)
  df <- copy(X[indices])
  setDT(df)
  df[, y := ys$excursion_high[indices]]
  df <- df |> as.data.frame() |> tidyr::drop_na(y)
  df
}

build_evt_df <- function(df_raw, threshold, feature_names) {
  if (is.null(df_raw)) return(NULL)
  df_evt <- df_raw |>
    dplyr::filter(y > threshold) |>
    dplyr::mutate(excess = y - threshold) |>
    dplyr::select(dplyr::all_of(c("excess", feature_names))) |>
    tidyr::drop_na()
  if (!nrow(df_evt)) return(NULL)
  df_evt
}

safe_evt_quantile <- function(alpha, threshold, sigma, xi) {
  sigma <- as.numeric(sigma)
  xi <- as.numeric(xi)
  valid <- is.finite(sigma) & is.finite(xi) & sigma > 0
  out <- rep(NA_real_, length(sigma))
  if (!any(valid)) return(out)

  sigma_v <- sigma[valid]
  xi_v <- xi[valid]
  near_zero <- abs(xi_v) < 1e-6

  if (any(!near_zero)) {
    nz <- !near_zero
    out_idx <- which(valid)[nz]
    out[out_idx] <- threshold + (sigma_v[nz] / xi_v[nz]) * ((1 - alpha)^(-xi_v[nz]) - 1)
  }

  if (any(near_zero)) {
    nz0 <- near_zero
    out_idx0 <- which(valid)[nz0]
    out[out_idx0] <- threshold + sigma_v[nz0] * log(1 / (1 - alpha))
  }

  out
}

summarize_return_levels <- function(return_matrix, probs) {
  safe_quantile <- function(values, prob) {
    stats::quantile(values, prob, na.rm = TRUE, names = FALSE)
  }
  summary_stats <- apply(return_matrix, 2, function(values) {
    c(
      mean = mean(values, na.rm = TRUE),
      median = stats::median(values, na.rm = TRUE),
      p90 = safe_quantile(values, 0.9),
      p99 = safe_quantile(values, 0.99),
      max = max(values, na.rm = TRUE)
    )
  })
  tibble(
    prob = probs,
    mean = summary_stats["mean", ],
    median = summary_stats["median", ],
    p90 = summary_stats["p90", ],
    p99 = summary_stats["p99", ],
    max = summary_stats["max", ]
  )
}

plot_pit_histogram <- function(pit_values, title_text, subtitle_text = NULL, bins = 40L) {
  if (!length(pit_values)) return(invisible(NULL))
  ggplot(tibble(pit = pit_values), aes(x = pit)) +
    geom_histogram(bins = bins, fill = "#3182bd", color = "white", alpha = 0.85) +
    geom_hline(yintercept = length(pit_values) / bins, linetype = "dashed", color = "#e6550d") +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = "PIT",
      y = "Count"
    ) +
    theme_minimal()
}

score_evt_dataset <- function(df_raw,
                              fit,
                              threshold,
                              feature_names,
                              label = "dataset",
                              probs = c(0.97, 0.98, 0.99, 0.995, 0.999)) {
  evt_df <- build_evt_df(df_raw, threshold, feature_names)
  if (is.null(evt_df)) {
    message(sprintf("No rows above threshold for %s.", label))
    return(NULL)
  }

  preds <- predict(fit, newdata = evt_df, type = "response")
  sigma_hat <- preds[, 1]
  xi_hat <- preds[, 2]

  pit <- pmin(pmax((1 + xi_hat * evt_df$excess / sigma_hat)^(-1 / xi_hat), 1e-12), 1 - 1e-12)

  return_matrix <- vapply(
    probs,
    function(prob) safe_evt_quantile(prob, threshold, sigma_hat, xi_hat),
    numeric(nrow(evt_df))
  )

  summary_tbl <- summarize_return_levels(return_matrix, probs)

  cat(sprintf("\nEVT quantile summary for %s:\n", label))
  print(summary_tbl)

  plt <- plot_pit_histogram(
    pit,
    title_text = sprintf("PIT Histogram – %s EVT Dataset", label),
    subtitle_text = sprintf("threshold = %.4f", threshold)
  )
  print(plt)

  list(
    dataset = label,
    threshold = threshold,
    evt_df = evt_df,
    sigma_hat = sigma_hat,
    xi_hat = xi_hat,
    pit = pit,
    return_summary = summary_tbl,
    pit_plot = plt
  )
}

# ------------------------------------------------------------------------------
# Main pipeline ----------------------------------------------------------------
# ------------------------------------------------------------------------------

evt_formula_string <- "excess ~ ae_volatility_vel_0 + close_signal_ratio_1 + signal_fast_ratio + smoothed_close + smoothed_close_vel_1"
evt_formula <- stats::as.formula(evt_formula_string)
evt_features <- setdiff(all.vars(evt_formula), "excess")
evt_threshold_quantile <- 0.97

df_train_raw <- build_base_df(train_indices)
df_val_raw <- build_base_df(val_indices)
df_test_raw <- build_base_df(test_indices)

evt_threshold <- stats::quantile(df_train_raw$y, evt_threshold_quantile, na.rm = TRUE)
evt_train_df <- build_evt_df(df_train_raw, evt_threshold, evt_features)
if (is.null(evt_train_df)) {
  stop("Training EVT dataset is empty; check threshold or data preparation.")
}

evt_fit <- vglm(evt_formula, gpd(), data = evt_train_df)
attr(evt_fit, "threshold_u") <- evt_threshold
attr(evt_fit, "formula_string") <- evt_formula_string

pipeline_results <- list()
pipeline_results$train <- score_evt_dataset(df_train_raw, evt_fit, evt_threshold, evt_features, label = "train")
if (!is.null(df_val_raw)) {
  pipeline_results$val <- score_evt_dataset(df_val_raw, evt_fit, evt_threshold, evt_features, label = "val")
}
if (!is.null(df_test_raw)) {
  pipeline_results$test <- score_evt_dataset(df_test_raw, evt_fit, evt_threshold, evt_features, label = "test")
}

assign("evt_pipeline_results", pipeline_results, envir = .GlobalEnv)
assign("evt_fit_model", evt_fit, envir = .GlobalEnv)
assign("evt_threshold", evt_threshold, envir = .GlobalEnv)
assign("evt_formula_features", evt_features, envir = .GlobalEnv)
cat("\nPipeline complete. Results stored in evt_pipeline_results.\n")
