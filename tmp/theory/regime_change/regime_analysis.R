# Regime Change Model Quality Assessment
# This script provides comprehensive analysis tools for evaluating rcfit models

library("dplyr")
library("ggplot2")
library("gridExtra")
library("reshape2")

# ============================================================================
# 1. TRANSITION MATRIX ANALYSIS
# ============================================================================

#' Calculate transition matrix for regime changes
#' @param fit rcfit object
#' @return Transition probability matrix
regime_transition_matrix <- function(fit) {
  regimes <- as.numeric(fit$regime_indicators)
  n_regimes <- fit$n_regimes
  
  # Initialize transition count matrix
  trans_matrix <- matrix(0, nrow = n_regimes, ncol = n_regimes)
  
  # Count transitions
  for (i in 1:(length(regimes) - 1)) {
    from_regime <- regimes[i]
    to_regime <- regimes[i + 1]
    trans_matrix[from_regime, to_regime] <- trans_matrix[from_regime, to_regime] + 1
  }
  
  # Convert to probabilities
  row_sums <- rowSums(trans_matrix)
  trans_prob <- trans_matrix / row_sums
  trans_prob[is.nan(trans_prob)] <- 0
  
  rownames(trans_prob) <- paste0("Regime_", 1:n_regimes)
  colnames(trans_prob) <- paste0("Regime_", 1:n_regimes)
  
  list(
    counts = trans_matrix,
    probabilities = trans_prob,
    persistence = diag(trans_prob)
  )
}

# ============================================================================
# 2. REGIME STATISTICS
# ============================================================================

#' Calculate statistics for each regime
#' @param fit rcfit object
#' @return Data frame with regime statistics
regime_statistics <- function(fit) {
  regimes <- as.numeric(fit$regime_indicators)
  residuals <- as.numeric(fit$residuals)
  series <- as.numeric(fit$original_series)
  
  stats_list <- lapply(1:fit$n_regimes, function(r) {
    regime_mask <- regimes == r
    regime_data <- series[regime_mask]
    regime_residuals <- residuals[regime_mask]
    
    # Calculate runs (consecutive periods in regime)
    runs <- rle(regimes == r)
    regime_runs <- runs$lengths[runs$values]
    
    data.frame(
      regime = r,
      n_observations = sum(regime_mask),
      pct_total = sum(regime_mask) / length(regimes) * 100,
      mean_value = mean(regime_data, na.rm = TRUE),
      sd_value = sd(regime_data, na.rm = TRUE),
      mean_residual = mean(regime_residuals, na.rm = TRUE),
      sd_residual = sd(regime_residuals, na.rm = TRUE),
      rmse = sqrt(mean(regime_residuals^2, na.rm = TRUE)),
      mae = mean(abs(regime_residuals), na.rm = TRUE),
      n_switches = length(regime_runs),
      avg_duration = mean(regime_runs),
      max_duration = max(regime_runs),
      min_duration = min(regime_runs)
    )
  })
  
  do.call(rbind, stats_list)
}

# ============================================================================
# 3. MODEL QUALITY METRICS
# ============================================================================

#' Calculate comprehensive quality metrics
#' @param fit rcfit object
#' @return List of quality metrics
model_quality_metrics <- function(fit) {
  series <- as.numeric(fit$original_series)
  fitted <- as.numeric(fit$fitted_values)
  residuals <- as.numeric(fit$residuals)
  n <- length(series)
  
  # Overall fit metrics
  ss_total <- sum((series - mean(series))^2)
  ss_residual <- sum(residuals^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  # Adjusted R-squared
  k <- fit$n_regimes * 2  # Approximate number of parameters
  adj_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - k - 1))
  
  # Error metrics
  mae <- mean(abs(residuals))
  rmse <- sqrt(mean(residuals^2))
  mape <- mean(abs(residuals / series)) * 100
  
  # Information criteria (approximate)
  aic <- n * log(ss_residual / n) + 2 * k
  bic <- n * log(ss_residual / n) + k * log(n)
  
  # Residual diagnostics
  ljung_box <- Box.test(residuals, lag = 10, type = "Ljung-Box")
  
  list(
    r_squared = r_squared,
    adj_r_squared = adj_r_squared,
    mae = mae,
    rmse = rmse,
    mape = mape,
    aic = aic,
    bic = bic,
    ljung_box_stat = ljung_box$statistic,
    ljung_box_pvalue = ljung_box$p.value,
    n_observations = n,
    n_regimes = fit$n_regimes,
    n_parameters = k
  )
}

# ============================================================================
# 4. VISUALIZATION FUNCTIONS
# ============================================================================

#' Plot time series with regime coloring
#' @param fit rcfit object
#' @param title Plot title
plot_regimes <- function(fit, title = "Time Series with Regime Identification") {
  df <- data.frame(
    date = zoo::index(fit$original_series),
    actual = as.numeric(fit$original_series),
    fitted = as.numeric(fit$fitted_values),
    regime = as.factor(as.numeric(fit$regime_indicators))
  )
  
  ggplot(df, aes(x = date)) +
    geom_line(aes(y = actual, color = "Actual"), linewidth = 0.8) +
    geom_line(aes(y = fitted, color = "Fitted"), linewidth = 0.6, linetype = "dashed") +
    geom_point(aes(y = actual, fill = regime), shape = 21, size = 1.5, alpha = 0.6) +
    scale_color_manual(values = c("Actual" = "black", "Fitted" = "red")) +
    scale_fill_brewer(palette = "Set1", name = "Regime") +
    labs(title = title, x = "Date", y = "Value", color = "Series") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

#' Plot residuals with regime coloring
#' @param fit rcfit object
plot_residuals <- function(fit) {
  df <- data.frame(
    date = zoo::index(fit$original_series),
    residuals = as.numeric(fit$residuals),
    regime = as.factor(as.numeric(fit$regime_indicators))
  )
  
  ggplot(df, aes(x = date, y = residuals)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(color = "gray30", linewidth = 0.5) +
    geom_point(aes(fill = regime), shape = 21, size = 2, alpha = 0.7) +
    scale_fill_brewer(palette = "Set1", name = "Regime") +
    labs(title = "Residuals Over Time", x = "Date", y = "Residuals") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

#' Plot residual diagnostics
#' @param fit rcfit object
plot_residual_diagnostics <- function(fit) {
  residuals <- as.numeric(fit$residuals)
  
  # QQ plot
  p1 <- ggplot(data.frame(residuals = residuals), aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = "Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  
  # Histogram
  p2 <- ggplot(data.frame(residuals = residuals), aes(x = residuals)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
    geom_density(aes(y = after_stat(count)), color = "red", linewidth = 1) +
    labs(title = "Residual Distribution", x = "Residuals", y = "Count") +
    theme_minimal()
  
  # ACF plot
  acf_data <- acf(residuals, plot = FALSE, lag.max = 20)
  acf_df <- data.frame(
    lag = acf_data$lag[-1],
    acf = acf_data$acf[-1]
  )
  
  ci <- qnorm((1 + 0.95) / 2) / sqrt(length(residuals))
  
  p3 <- ggplot(acf_df, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = c(-ci, ci), linetype = "dashed", color = "blue") +
    geom_segment(aes(xend = lag, yend = 0), color = "steelblue", linewidth = 1) +
    labs(title = "ACF of Residuals", x = "Lag", y = "ACF") +
    theme_minimal()
  
  # Residuals vs Fitted
  p4 <- ggplot(data.frame(
    fitted = as.numeric(fit$fitted_values),
    residuals = residuals
  ), aes(x = fitted, y = residuals)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(alpha = 0.5, color = "steelblue") +
    geom_smooth(method = "loess", color = "red", se = FALSE) +
    labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
    theme_minimal()
  
  gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
}

#' Plot regime durations
#' @param fit rcfit object
plot_regime_durations <- function(fit) {
  regimes <- as.numeric(fit$regime_indicators)
  
  # Calculate runs for each regime
  runs_list <- lapply(1:fit$n_regimes, function(r) {
    runs <- rle(regimes == r)
    regime_runs <- runs$lengths[runs$values]
    data.frame(regime = as.factor(r), duration = regime_runs)
  })
  
  runs_df <- do.call(rbind, runs_list)
  
  ggplot(runs_df, aes(x = duration, fill = regime)) +
    geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
    facet_wrap(~regime, scales = "free_y") +
    scale_fill_brewer(palette = "Set1") +
    labs(title = "Regime Duration Distribution", 
         x = "Duration (periods)", 
         y = "Count") +
    theme_minimal() +
    theme(legend.position = "none")
}

#' Visualize feature space clustering
#' @param fit rcfit object
plot_feature_space <- function(fit) {
  if (is.null(fit$features)) {
    warning("No feature data available in fit object")
    return(NULL)
  }
  
  df <- data.frame(
    vol = fit$features[, 1],
    mean = fit$features[, 2],
    regime = as.factor(as.numeric(fit$regime_indicators))
  )
  
  ggplot(df, aes(x = vol, y = mean, color = regime)) +
    geom_point(alpha = 0.6, size = 2) +
    scale_color_brewer(palette = "Set1", name = "Regime") +
    labs(title = "Feature Space Clustering",
         x = "Rolling Volatility (scaled)",
         y = "Rolling Mean Return (scaled)") +
    theme_minimal()
}

#' Plot transition matrix heatmap
#' @param trans_matrix Transition matrix from regime_transition_matrix()
plot_transition_matrix <- function(trans_matrix) {
  trans_df <- melt(trans_matrix$probabilities)
  colnames(trans_df) <- c("From", "To", "Probability")
  
  ggplot(trans_df, aes(x = To, y = From, fill = Probability)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", Probability)), color = "white", size = 5) +
    scale_fill_gradient2(low = "navy", mid = "steelblue", high = "red", 
                         midpoint = 0.5, limits = c(0, 1)) +
    labs(title = "Regime Transition Probability Matrix",
         x = "To Regime", y = "From Regime") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# ============================================================================
# 5. COMBINED REGIME ANALYSIS
# ============================================================================

#' Analyze combined regimes (trend and volatility)
#' @param trend_fit Trend regime fit object
#' @param vol_fit Volatility regime fit object
analyze_combined_regimes <- function(trend_fit, vol_fit) {
  trend_regimes <- as.numeric(trend_fit$regime_indicators)
  vol_regimes <- as.numeric(vol_fit$regime_indicators)
  
  # Ensure same length
  min_len <- min(length(trend_regimes), length(vol_regimes))
  trend_regimes <- trend_regimes[1:min_len]
  vol_regimes <- vol_regimes[1:min_len]
  
  # Joint distribution
  joint_table <- table(
    Trend = trend_regimes,
    Volatility = vol_regimes
  )
  
  joint_prop <- prop.table(joint_table)
  
  # Create state labels
  states <- paste0("T", trend_regimes, "-V", vol_regimes)
  state_table <- table(states)
  
  list(
    joint_counts = joint_table,
    joint_proportions = joint_prop,
    state_counts = state_table,
    n_unique_states = length(unique(states))
  )
}

#' Plot combined regime heatmap
#' @param trend_fit Trend regime fit object
#' @param vol_fit Volatility regime fit object
plot_combined_regimes <- function(trend_fit, vol_fit) {
  combined <- analyze_combined_regimes(trend_fit, vol_fit)
  
  joint_df <- melt(combined$joint_proportions)
  colnames(joint_df) <- c("Trend", "Volatility", "Proportion")
  
  ggplot(joint_df, aes(x = Volatility, y = Trend, fill = Proportion)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.3f", Proportion)), color = "white", size = 4) +
    scale_fill_gradient2(low = "white", mid = "steelblue", high = "darkred", 
                         midpoint = max(joint_df$Proportion) / 2) +
    labs(title = "Joint Distribution: Trend vs Volatility Regimes",
         x = "Volatility Regime", y = "Trend Regime") +
    theme_minimal()
}

# ============================================================================
# 6. COMPREHENSIVE REPORT FUNCTION
# ============================================================================

#' Generate comprehensive quality assessment report
#' @param fit rcfit object
#' @param name Model name for labeling
#' @return List containing all metrics and plots
regime_quality_report <- function(fit, name = "Model") {
  cat("\n", rep("=", 70), "\n", sep = "")
  cat("REGIME CHANGE MODEL QUALITY REPORT:", name, "\n")
  cat(rep("=", 70), "\n\n", sep = "")
  
  # 1. Transition Matrix
  cat("1. TRANSITION MATRIX\n")
  cat(rep("-", 70), "\n", sep = "")
  trans_matrix <- regime_transition_matrix(fit)
  print(round(trans_matrix$probabilities, 3))
  cat("\nRegime Persistence (diagonal):\n")
  print(round(trans_matrix$persistence, 3))
  cat("\n")
  
  # 2. Regime Statistics
  cat("2. REGIME STATISTICS\n")
  cat(rep("-", 70), "\n", sep = "")
  regime_stats <- regime_statistics(fit)
  print(regime_stats)
  cat("\n")
  
  # 3. Model Quality Metrics
  cat("3. MODEL QUALITY METRICS\n")
  cat(rep("-", 70), "\n", sep = "")
  quality <- model_quality_metrics(fit)
  cat(sprintf("R-squared: %.4f\n", quality$r_squared))
  cat(sprintf("Adjusted R-squared: %.4f\n", quality$adj_r_squared))
  cat(sprintf("MAE: %.6f\n", quality$mae))
  cat(sprintf("RMSE: %.6f\n", quality$rmse))
  cat(sprintf("MAPE: %.2f%%\n", quality$mape))
  cat(sprintf("AIC: %.2f\n", quality$aic))
  cat(sprintf("BIC: %.2f\n", quality$bic))
  cat(sprintf("Ljung-Box statistic: %.4f (p-value: %.4f)\n", 
              quality$ljung_box_stat, quality$ljung_box_pvalue))
  cat("\n")
  
  # Return all results
  list(
    transition_matrix = trans_matrix,
    regime_statistics = regime_stats,
    quality_metrics = quality,
    plots = list(
      regimes = plot_regimes(fit, paste(name, "- Regimes")),
      residuals = plot_residuals(fit),
      diagnostics = plot_residual_diagnostics(fit),
      durations = plot_regime_durations(fit),
      features = plot_feature_space(fit),
      transition_heatmap = plot_transition_matrix(trans_matrix)
    )
  )
}
