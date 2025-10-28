### Caching

The `prepare_fwd()` function supports caching to speed up repeated runs with the same parameters:

```r
# Enable caching
features <- prepare_fwd(
  fetl,
  methods = c("extreme_high_identity", "mass_low_log", "close_identity"),
  days = 20,
  companies = 500,
  cache = TRUE  # Enable caching
)
```

**How it works:**
- Cache files are stored in `_cache_/` directory (automatically created)
- Cache key is based on MD5 hash of parameters: `methods`, `days`, and `companies`
- When `cache = TRUE` and matching cache exists, data is loaded from cache
- When `cache = TRUE` and no cache exists, data is computed and saved to cache
- The `digest` package is used for generating cache keys

**Benefits:**
- Significantly reduces computation time for repeated runs
- Useful during model development and experimentation
- Cache files use `.rds` format for efficient storage

**Note:** The `_cache_/` directory is git-ignored to avoid committing large cache files.

### Preprocessing

Inputs

- Triple MAs using Kalman filter on the signal
- Use GARCH and volatility metrics on the series
- Use technical indicators SMA (relative) and RSI on the series
- Add price to SMAs
- Add velocity and acceleration `*_0`, `*_1` to previous indicators
- Perhaps `2`-, `3`-point forecast
- SOM neural pattern (use mixed models to find the ts pattern)

Dependent Variable

- Response variable has to be `ln(sum(mass upside) - sum(mass downside))`

Data

- Single time series; joint (normalised) time series

### Model

- Stacked ensemble of base learners GBTM (XGBoost), DL (TNC, etc), Hyperplane (GLM, SVM), meta learner RF
- Include SOM neural pattern and current volatility in meta learner
- Train on OOF set
