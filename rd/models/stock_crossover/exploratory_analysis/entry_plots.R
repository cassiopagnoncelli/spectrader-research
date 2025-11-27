#
# PLOTS.
#
if (FALSE) {
  plot_metrics_comparison(model_signal)
  plot_feature_importance(model_signal, top_n = 20)
  plot_all_predictions(model_signal)
  plot_predictions_vs_actuals(model_signal, "test")
  plot_residuals(model_signal, "test")
  plot_residual_distribution(model_signal, "test")
  plot_predictions_vs_actuals(model_signal, "val")
  plot_residuals(model_signal, "val")
  plot_predictions_vs_actuals(model_signal, "train")
  plot_residuals(model_signal, "train")
  if (FALSE) {
    plot_xgboost_trees(model_signal, tree_indices = c(0, 1, 2, 3, 4))
  }
}

# Plot predicted vs actual values
plot_predictions_vs_actuals <- function(results, split = "test", max_points = 10000) {
  predictions <- results$predictions[[split]]
  actuals <- results$actuals[[split]]

  df <- data.frame(
    actual = actuals,
    predicted = predictions
  )

  # Sample if too many points
  if (nrow(df) > max_points) {
    set.seed(123)
    df <- df[sample(nrow(df), max_points), ]
  }

  # Calculate R²
  r2 <- results$metrics[[split]]["r2"]
  rmse <- results$metrics[[split]]["rmse"]

  subtitle_text <- sprintf("R² = %.4f, RMSE = %.4f", r2, rmse)
  if (nrow(df) == max_points) {
    subtitle_text <- sprintf("%s (sampled %d points)", subtitle_text, max_points)
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = actual, y = predicted)) +
    ggplot2::geom_point(alpha = 0.3, color = "steelblue") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
    ggplot2::labs(
      title = sprintf("%s Set: Predicted vs Actual", tools::toTitleCase(split)),
      subtitle = subtitle_text,
      x = "Actual Value",
      y = "Predicted Value"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 11, color = "gray40")
    )

  p
}

# Plot residuals
plot_residuals <- function(results, split = "test", max_points = 10000) {
  predictions <- results$predictions[[split]]
  actuals <- results$actuals[[split]]
  residuals <- actuals - predictions

  df <- data.frame(
    predicted = predictions,
    residuals = residuals
  )

  # Sample if too many points to speed up plotting
  if (nrow(df) > max_points) {
    set.seed(123)
    df <- df[sample(nrow(df), max_points), ]
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = predicted, y = residuals)) +
    ggplot2::geom_point(alpha = 0.3, color = "steelblue") +
    ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
    ggplot2::labs(
      title = sprintf("%s Set: Residual Plot", tools::toTitleCase(split)),
      subtitle = ifelse(nrow(df) == max_points,
        sprintf("(sampled %d points)", max_points),
        ""
      ),
      x = "Predicted Value",
      y = "Residuals (Actual - Predicted)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10, color = "gray40")
    )

  p
}

# Plot residual distribution
plot_residual_distribution <- function(results, split = "test", max_points = 50000) {
  predictions <- results$predictions[[split]]
  actuals <- results$actuals[[split]]
  residuals <- actuals - predictions

  df <- data.frame(residuals = residuals)

  # Sample if too many points for density calculation
  if (nrow(df) > max_points) {
    set.seed(123)
    df <- df[sample(nrow(df), max_points), , drop = FALSE]
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = residuals)) +
    ggplot2::geom_histogram(aes(y = ggplot2::after_stat(density)), bins = 50, fill = "steelblue", alpha = 0.7) +
    ggplot2::geom_density(color = "red", linewidth = 1) +
    ggplot2::geom_vline(xintercept = 0, color = "darkgreen", linetype = "dashed", linewidth = 1) +
    ggplot2::labs(
      title = sprintf("%s Set: Residual Distribution", tools::toTitleCase(split)),
      x = "Residuals",
      y = "Density"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )

  p
}

# Plot feature importance
plot_feature_importance <- function(results, top_n = 20) {
  importance_df <- head(results$importance, top_n)

  # Reorder features by Gain
  importance_df$Feature <- factor(importance_df$Feature, levels = rev(importance_df$Feature))

  p <- ggplot2::ggplot(importance_df, ggplot2::aes(x = Gain, y = Feature)) +
    ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
    ggplot2::labs(
      title = sprintf("Top %d Feature Importance (by Gain)", top_n),
      x = "Gain",
      y = "Feature"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 9)
    )

  return(p)
}

# Plot metrics comparison across splits
plot_metrics_comparison <- function(results) {
  metrics_df <- data.frame(
    Split = rep(c("Train", "Val", "Test"), each = 3),
    Metric = rep(c("RMSE", "MAE", "R²"), 3),
    Value = c(
      results$metrics$train["rmse"],
      results$metrics$train["mae"],
      results$metrics$train["r2"],
      results$metrics$val["rmse"],
      results$metrics$val["mae"],
      results$metrics$val["r2"],
      results$metrics$test["rmse"],
      results$metrics$test["mae"],
      results$metrics$test["r2"]
    )
  )

  metrics_df$Split <- factor(metrics_df$Split, levels = c("Train", "Val", "Test"))

  p <- ggplot2::ggplot(metrics_df, ggplot2::aes(x = Split, y = Value, fill = Split)) +
    ggplot2::geom_col(alpha = 0.8) +
    ggplot2::facet_wrap(~Metric, scales = "free_y", ncol = 3) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.4f", Value)), vjust = -0.5, size = 3.5) +
    ggplot2::scale_fill_manual(values = c("Train" = "#2E86AB", "Val" = "#A23B72", "Test" = "#F18F01")) +
    ggplot2::labs(
      title = "Performance Metrics Across Data Splits",
      x = "",
      y = "Metric Value"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.position = "none",
      strip.text = ggplot2::element_text(size = 11, face = "bold")
    )

  p
}

# Combined plot for a specific split
plot_split_analysis <- function(results, split = "test") {
  p1 <- plot_predictions_vs_actuals(results, split)
  p2 <- plot_residuals(results, split)
  p3 <- plot_residual_distribution(results, split)

  # Combine plots
  combined <- gridExtra::grid.arrange(
    p1, p2, p3,
    ncol = 2, nrow = 2, top = sprintf("%s Set Analysis", tools::toTitleCase(split))
  )

  return(combined)
}

# Plot XGBoost trees
plot_xgboost_trees <- function(results, tree_indices = c(0, 1, 2)) {
  # Check if model exists in results (support both 'model' and 'model_final')
  model <- NULL
  if (!is.null(results$model)) {
    model <- results$model
  } else if (!is.null(results$model_final)) {
    model <- results$model_final
  }

  if (is.null(model)) {
    warning("No model found in results. Cannot plot trees.")
    return(NULL)
  }

  # Get feature names from importance data if available
  feature_names <- NULL
  if (!is.null(results$importance)) {
    feature_names <- results$importance$Feature
  }

  # Create list to store plots
  plots <- list()

  # Plot each tree
  for (tree_idx in tree_indices) {
    cat(sprintf("\n=== Tree %d ===\n", tree_idx))

    tryCatch(
      {
        # Generate and display tree plot
        tree_plot <- xgboost::xgb.plot.tree(
          model = model,
          trees = tree_idx,
          feature_names = feature_names
        )

        plots[[as.character(tree_idx)]] <- tree_plot
      },
      error = function(e) {
        warning(sprintf("Failed to plot tree %d: %s", tree_idx, e$message))
      }
    )
  }

  plots
}

# Plot all predictions vs actuals in one figure
plot_all_predictions <- function(results, max_points_per_split = 5000) {
  # Sample each split separately to maintain balance
  sample_split <- function(actuals, predictions, split_name, max_points) {
    df <- data.frame(actual = actuals, predicted = predictions)
    if (nrow(df) > max_points) {
      set.seed(123)
      df <- df[sample(nrow(df), max_points), ]
    }
    df$split <- split_name
    df
  }

  # Combine all data with sampling
  all_data <- rbind(
    sample_split(results$actuals$train, results$predictions$train, "Train", max_points_per_split),
    sample_split(results$actuals$val, results$predictions$val, "Val", max_points_per_split),
    sample_split(results$actuals$test, results$predictions$test, "Test", max_points_per_split)
  )

  all_data$split <- factor(all_data$split, levels = c("Train", "Val", "Test"))

  p <- ggplot2::ggplot(all_data, ggplot2::aes(x = actual, y = predicted, color = split)) +
    ggplot2::geom_point(alpha = 0.3) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
    ggplot2::facet_wrap(~split, ncol = 3) +
    ggplot2::scale_color_manual(values = c("Train" = "#2E86AB", "Val" = "#A23B72", "Test" = "#F18F01")) +
    ggplot2::labs(
      title = "Predicted vs Actual Across All Splits",
      subtitle = sprintf("(sampled %d points per split)", max_points_per_split),
      x = "Actual Value",
      y = "Predicted Value"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10, color = "gray40"),
      legend.position = "none",
      strip.text = ggplot2::element_text(size = 11, face = "bold")
    )

  return(p)
}
