# Regime Change Model Quality Assessment

Comprehensive analysis toolkit for evaluating the quality of regime change models (`trend_fit` and `vol_fit`).

## Quick Start

```r
# 1. First, run your regime models
source("tmp/theory/regime_change.R")

# 2. Source the analysis functions
source("tmp/theory/regime_analysis.R")

# 3. Run comprehensive analysis
source("tmp/theory/run_regime_analysis.R")
```

## Available Analysis Methods

### 1. Transition Matrix Analysis

**Purpose:** Understand regime switching behavior and persistence

```r
# Calculate transition probabilities
trans_matrix <- regime_transition_matrix(trend_fit)

# Access different components:
trans_matrix$counts          # Raw transition counts
trans_matrix$probabilities   # Transition probability matrix
trans_matrix$persistence     # Diagonal elements (staying in same regime)

# Visualize
plot_transition_matrix(trans_matrix)
```

**What to look for:**
- High diagonal values = stable regimes (good)
- Low diagonal values = frequent switching (potentially overfitting)
- Off-diagonal patterns show regime switching tendencies

---

### 2. Regime Statistics

**Purpose:** Characterize each regime's behavior and duration

```r
stats <- regime_statistics(trend_fit)
print(stats)
```

**Columns include:**
- `n_observations`: Number of periods in each regime
- `pct_total`: Percentage of total time in each regime
- `mean_value`, `sd_value`: Data characteristics per regime
- `rmse`, `mae`: Fit quality per regime
- `avg_duration`, `max_duration`: How long regimes last

---

### 3. Model Quality Metrics

**Purpose:** Overall model fit assessment

```r
quality <- model_quality_metrics(trend_fit)
```

**Metrics provided:**
- **R-squared**: Overall explanatory power (higher = better)
- **Adjusted R-squared**: Penalized for model complexity
- **MAE/RMSE**: Average prediction errors (lower = better)
- **MAPE**: Mean absolute percentage error
- **AIC/BIC**: Information criteria for model comparison (lower = better)
- **Ljung-Box test**: Tests for autocorrelation in residuals (p > 0.05 is good)

---

### 4. Visualization Suite

#### Time Series with Regime Coloring
```r
plot_regimes(trend_fit, "My Custom Title")
```
Shows actual vs fitted values with regime periods color-coded.

#### Residual Analysis
```r
plot_residuals(trend_fit)
```
Visualizes residuals over time by regime.

#### Diagnostic Plots (4-panel)
```r
plot_residual_diagnostics(trend_fit)
```
Includes:
- Q-Q plot (normality check)
- Histogram of residuals
- ACF plot (autocorrelation)
- Residuals vs Fitted (heteroscedasticity check)

#### Regime Duration Distribution
```r
plot_regime_durations(trend_fit)
```
Histograms showing how long each regime typically lasts.

#### Feature Space Clustering
```r
plot_feature_space(trend_fit)
```
Shows how the k-means clustering separated the regimes based on volatility and mean returns.

---

### 5. Combined Regime Analysis

**Purpose:** Analyze interactions between trend and volatility regimes

```r
# Statistical analysis
combined <- analyze_combined_regimes(trend_fit, vol_fit)
print(combined$joint_counts)        # How often each combination occurs
print(combined$joint_proportions)   # As percentages

# Visualization
plot_combined_regimes(trend_fit, vol_fit)
```

**Interpretation:**
- Identifies market "states" (e.g., "bullish trend + low volatility")
- Shows which combinations are common vs rare
- Helps understand regime coherence

---

### 6. Comprehensive Report

**Purpose:** Generate everything at once

```r
report <- regime_quality_report(trend_fit, name = "Trend Model")
```

**Returns:**
- `report$transition_matrix`: Transition analysis
- `report$regime_statistics`: Per-regime stats
- `report$quality_metrics`: Overall fit metrics
- `report$plots`: All visualizations in a list

Prints formatted report to console.

---

## Interpretation Guide

### Good Model Indicators
✅ **High R-squared** (> 0.7): Model explains most variation
✅ **High persistence** (> 0.8): Regimes are stable, not random
✅ **Low RMSE**: Predictions are accurate
✅ **Ljung-Box p > 0.05**: No residual autocorrelation
✅ **Normal residuals**: Q-Q plot follows diagonal line
✅ **Distinct regimes**: Clear separation in feature space plot

### Warning Signs
⚠️ **Low persistence** (< 0.5): Too much regime switching
⚠️ **Ljung-Box p < 0.05**: Residuals show patterns (model missing something)
⚠️ **Non-normal residuals**: Q-Q plot deviates significantly
⚠️ **Heteroscedasticity**: Residuals vs Fitted shows patterns
⚠️ **Very short regime durations**: Might be overfitting noise

---

## Model Comparison

Compare multiple models:

```r
# Generate reports
trend_report <- regime_quality_report(trend_fit, "Trend")
vol_report <- regime_quality_report(vol_fit, "Volatility")

# Compare metrics side by side
comparison <- data.frame(
  Metric = c("R-squared", "RMSE", "AIC", "BIC"),
  Trend = c(
    trend_report$quality_metrics$r_squared,
    trend_report$quality_metrics$rmse,
    trend_report$quality_metrics$aic,
    trend_report$quality_metrics$bic
  ),
  Volatility = c(
    vol_report$quality_metrics$r_squared,
    vol_report$quality_metrics$rmse,
    vol_report$quality_metrics$aic,
    vol_report$quality_metrics$bic
  )
)
print(comparison)
```

---

## Saving Results

```r
# Save RDS objects
saveRDS(trend_report, "trend_report.rds")

# Save individual plots
library(ggplot2)
ggsave("regime_plot.png", trend_report$plots$regimes, 
       width = 12, height = 6, dpi = 300)

# Export metrics to CSV
write.csv(trend_report$regime_statistics, "regime_stats.csv")
write.csv(trend_report$transition_matrix$probabilities, "transition_matrix.csv")
```

---

## Function Reference

| Function | Purpose | Returns |
|----------|---------|---------|
| `regime_transition_matrix()` | Calculate transition probabilities | List with counts, probabilities, persistence |
| `regime_statistics()` | Per-regime statistics | Data frame |
| `model_quality_metrics()` | Overall fit metrics | List of metrics |
| `plot_regimes()` | Time series with regime coloring | ggplot object |
| `plot_residuals()` | Residual time series | ggplot object |
| `plot_residual_diagnostics()` | 4-panel diagnostic plots | Grid arrangement |
| `plot_regime_durations()` | Duration histograms | ggplot object |
| `plot_feature_space()` | Clustering visualization | ggplot object |
| `plot_transition_matrix()` | Heatmap of transitions | ggplot object |
| `analyze_combined_regimes()` | Joint regime analysis | List |
| `plot_combined_regimes()` | Combined regime heatmap | ggplot object |
| `regime_quality_report()` | Generate full report | List with all analyses |

---

## Dependencies

Required packages:
- `dplyr`
- `ggplot2`
- `gridExtra`
- `reshape2`

All should be automatically loaded when sourcing `regime_analysis.R`.

---

## Tips

1. **Start with the comprehensive report** to get overview
2. **Check transition matrices** to understand regime stability
3. **Examine residual diagnostics** to validate assumptions
4. **Use combined analysis** to understand market states
5. **Compare AIC/BIC** when testing different numbers of regimes
6. **Save all plots** for documentation and presentations

---

## Example Workflow

```r
# 1. Load and fit models
source("tmp/theory/regime_change.R")

# 2. Load analysis tools
source("tmp/theory/regime_analysis.R")

# 3. Quick look at key metrics
quality <- model_quality_metrics(trend_fit)
print(quality$r_squared)
print(quality$rmse)

# 4. Check regime stability
trans <- regime_transition_matrix(trend_fit)
print(trans$persistence)

# 5. Visual inspection
plot_regimes(trend_fit)
plot_residual_diagnostics(trend_fit)

# 6. Full report if needed
report <- regime_quality_report(trend_fit, "My Model")

# 7. Combined analysis
combined <- analyze_combined_regimes(trend_fit, vol_fit)
plot_combined_regimes(trend_fit, vol_fit)
```

---

For questions or issues, refer to the function documentation in `regime_analysis.R`.
