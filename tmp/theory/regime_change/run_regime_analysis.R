# Example: Comprehensive Regime Change Model Analysis
# This script demonstrates how to analyze your trend_fit and vol_fit models

# Source the analysis functions
source("tmp/theory/regime_change/regime_analysis.R")

# Load your models (assuming they're already loaded from regime_change.R)
# If not, you would run: source("tmp/theory/regime_change.R")

# ============================================================================
# ANALYZE TREND MODEL
# ============================================================================

cat("\n\n")
cat("#######################################################################\n")
cat("# TREND REGIME ANALYSIS\n")
cat("#######################################################################\n")

trend_report <- regime_quality_report(trend_fit, name = "Trend Model")

# Display individual plots
cat("\nDisplaying Trend Model plots...\n")
print(trend_report$plots$regimes)
print(trend_report$plots$residuals)
trend_report$plots$diagnostics  # This uses grid.arrange internally
print(trend_report$plots$durations)
print(trend_report$plots$features)
print(trend_report$plots$transition_heatmap)

# ============================================================================
# ANALYZE VOLATILITY MODEL
# ============================================================================

cat("\n\n")
cat("#######################################################################\n")
cat("# VOLATILITY REGIME ANALYSIS\n")
cat("#######################################################################\n")

vol_report <- regime_quality_report(vol_fit, name = "Volatility Model")

# Display individual plots
cat("\nDisplaying Volatility Model plots...\n")
print(vol_report$plots$regimes)
print(vol_report$plots$residuals)
vol_report$plots$diagnostics  # This uses grid.arrange internally
print(vol_report$plots$durations)
print(vol_report$plots$features)
print(vol_report$plots$transition_heatmap)

# ============================================================================
# COMBINED REGIME ANALYSIS
# ============================================================================

cat("\n\n")
cat("#######################################################################\n")
cat("# COMBINED TREND-VOLATILITY REGIME ANALYSIS\n")
cat("#######################################################################\n")

# Analyze combined regimes
combined_analysis <- analyze_combined_regimes(trend_fit, vol_fit)

cat("\nJoint Distribution (counts):\n")
print(combined_analysis$joint_counts)

cat("\nJoint Distribution (proportions):\n")
print(round(combined_analysis$joint_proportions, 4))

cat("\nCombined State Counts:\n")
print(combined_analysis$state_counts)

cat("\nNumber of unique combined states:", combined_analysis$n_unique_states, "\n")

# Plot combined regimes heatmap
print(plot_combined_regimes(trend_fit, vol_fit))

# ============================================================================
# COMPARISON TABLE
# ============================================================================

cat("\n\n")
cat("#######################################################################\n")
cat("# MODEL COMPARISON\n")
cat("#######################################################################\n")

comparison <- data.frame(
  Metric = c("R-squared", "Adjusted R²", "MAE", "RMSE", "AIC", "BIC", 
             "Ljung-Box p-value", "N Observations", "N Regimes"),
  Trend_Model = c(
    trend_report$quality_metrics$r_squared,
    trend_report$quality_metrics$adj_r_squared,
    trend_report$quality_metrics$mae,
    trend_report$quality_metrics$rmse,
    trend_report$quality_metrics$aic,
    trend_report$quality_metrics$bic,
    trend_report$quality_metrics$ljung_box_pvalue,
    trend_report$quality_metrics$n_observations,
    trend_report$quality_metrics$n_regimes
  ),
  Volatility_Model = c(
    vol_report$quality_metrics$r_squared,
    vol_report$quality_metrics$adj_r_squared,
    vol_report$quality_metrics$mae,
    vol_report$quality_metrics$rmse,
    vol_report$quality_metrics$aic,
    vol_report$quality_metrics$bic,
    vol_report$quality_metrics$ljung_box_pvalue,
    vol_report$quality_metrics$n_observations,
    vol_report$quality_metrics$n_regimes
  )
)

print(comparison)

# ============================================================================
# INDIVIDUAL ANALYSIS EXAMPLES
# ============================================================================

cat("\n\n")
cat("#######################################################################\n")
cat("# ADDITIONAL INDIVIDUAL ANALYSES (Examples)\n")
cat("#######################################################################\n")

# Example 1: Get just the transition matrix
cat("\n--- Trend Transition Matrix ---\n")
trend_trans <- regime_transition_matrix(trend_fit)
print(round(trend_trans$probabilities, 3))

# Example 2: Get regime statistics only
cat("\n--- Volatility Regime Statistics ---\n")
vol_stats <- regime_statistics(vol_fit)
print(vol_stats)

# Example 3: Individual plots
cat("\n--- Creating individual visualization ---\n")
p1 <- plot_regimes(trend_fit, "Trend Model - Custom Title")
print(p1)

# Example 4: Feature space comparison
cat("\n--- Feature Space Plots ---\n")
print(plot_feature_space(trend_fit))
print(plot_feature_space(vol_fit))

# ============================================================================
# SAVE RESULTS (Optional)
# ============================================================================

# Uncomment to save results
# saveRDS(trend_report, "tmp/theory/trend_report.rds")
# saveRDS(vol_report, "tmp/theory/vol_report.rds")
# saveRDS(combined_analysis, "tmp/theory/combined_analysis.rds")

# Save plots as PNG
# ggsave("tmp/theory/trend_regimes.png", trend_report$plots$regimes, 
#        width = 12, height = 6, dpi = 300)
# ggsave("tmp/theory/vol_regimes.png", vol_report$plots$regimes, 
#        width = 12, height = 6, dpi = 300)
# ggsave("tmp/theory/combined_regimes.png", plot_combined_regimes(trend_fit, vol_fit),
#        width = 8, height = 6, dpi = 300)

cat("\n\n")
cat("#######################################################################\n")
cat("# ANALYSIS COMPLETE\n")
cat("#######################################################################\n")
cat("\nAll analysis results are stored in:\n")
cat("  - trend_report: Comprehensive trend model analysis\n")
cat("  - vol_report: Comprehensive volatility model analysis\n")
cat("  - combined_analysis: Joint trend-volatility analysis\n")
cat("  - comparison: Side-by-side model comparison table\n")
