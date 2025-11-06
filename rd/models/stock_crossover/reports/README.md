# Stock Crossover Trade Results Report

Simple R Markdown report to display trade simulation results.

## Usage

After running `regression.R` and `trade_simulation.R` to generate `dfsr` and `posl` objects:

```r
# In R console
source("rd/models/stock_crossover/report/render_report.R")
```

This will generate `trade_results.html` in the same directory.

## What's Displayed

The report shows:

1. **Trade Results Summary** - Position counts
2. **Kelly Criterion** - Optimal fraction calculation and plot
3. **Returns Distribution** - Visual distribution of returns
4. **Accuracy Analysis** - Broken down by:
   - Take Profit positions
   - Open positions
   - All positions
   
   Each section includes:
   - Exit metrics (win rate, avg return, Sharpe ratio, etc.)
   - Distribution analysis

## Parameters

The report accepts two parameters:
- `dfsr` - Position returns tibble from `position_cohort_return()`
- `posl` - List of position cohorts from `position_cohort()`

## Output

`trade_results.html` - Clean, formatted HTML report
