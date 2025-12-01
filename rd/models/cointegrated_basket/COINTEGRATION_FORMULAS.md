# Cointegration Linear Combinations Formulas

## Overview

When the Johansen test identifies `r` cointegrating relationships among `n` I(1) time series, it provides eigenvectors that define stationary linear combinations of the non-stationary series.

## Mathematical Framework

### The Vector Error Correction Model (VECM)

The Johansen test estimates a VECM of the form:

```
ΔY_t = α β' Y_{t-1} + Γ₁ ΔY_{t-1} + ... + Γ_{K-1} ΔY_{t-K+1} + μ + ε_t
```

Where:
- `Y_t` is an (n × 1) vector of price series at time t
- `β` is an (n × r) matrix of cointegrating vectors (eigenvectors)
- `α` is an (n × r) matrix of adjustment coefficients (loading matrix)
- `Γ_i` are (n × n) matrices of short-run dynamics
- `μ` is an (n × 1) vector of intercepts
- `ε_t` is an (n × 1) vector of errors

### Cointegrating Relationships

The cointegration relationships are given by:

```
z_t = β' Y_t
```

Where:
- `z_t` is an (r × 1) vector of stationary linear combinations
- Each row of `β'` defines one cointegrating relationship
- `β'` has dimensions (r × n), with r cointegrating vectors

## Practical Formula

For a system with n assets (e.g., 3 assets: X, Y, Z) and cointegration rank r = 1, the linear combination is:

```
z_t = β₁ X_t + β₂ Y_t + β₃ Z_t + c
```

Where:
- `β₁, β₂, β₃` are the cointegrating vector weights
- `c` is the intercept (may or may not be included depending on specification)
- `z_t` is a stationary process (mean-reverting spread)

### Multiple Cointegrating Relationships

If rank r = 2 with 3 assets:

```
z₁_t = β₁₁ X_t + β₂₁ Y_t + β₃₁ Z_t + c₁
z₂_t = β₁₂ X_t + β₂₂ Y_t + β₃₂ Z_t + c₂
```

Both `z₁_t` and `z₂_t` are stationary.

## Extracting Cointegrating Vectors from Johansen Test

### Using the `ca.jo` Object

```r
# After running test_cointegration_rank()
result <- test_cointegration_rank(price_matrix)
johansen <- result$johansen

# Extract cointegrating vectors (eigenvectors)
# These are normalized eigenvectors (first element = 1 for each vector)
beta <- johansen@V  # (n × n) matrix of eigenvectors

# Number of cointegrating relationships
r <- result$coint_rank

# Extract only the significant cointegrating vectors
# The last r columns of beta correspond to the r cointegrating relationships
coint_vectors <- beta[, (ncol(beta) - r + 1):ncol(beta), drop = FALSE]

# For interpretation, often we use the transpose
coint_vectors_t <- t(coint_vectors)  # (r × n) matrix
```

### Example with Real Data

```r
library(urca)

# Generate cointegrated series
set.seed(123)
n <- 200
x <- cumsum(rnorm(n))
y <- 2 * x + cumsum(rnorm(n, 0, 0.5))  # y ≈ 2x + noise
z <- 1.5 * x + cumsum(rnorm(n, 0, 0.5))  # z ≈ 1.5x + noise

price_matrix <- cbind(X = x, Y = y, Z = z)
colnames(price_matrix) <- c("X", "Y", "Z")

# Test cointegration
result <- test_cointegration_rank(price_matrix, verbose = TRUE)

# Extract cointegrating vectors
if (result$valid_for_coint && result$coint_rank > 0) {
  johansen <- result$johansen
  r <- result$coint_rank
  n_vars <- ncol(price_matrix)
  
  # Get eigenvectors (β matrix)
  beta <- johansen@V
  
  # Extract the r cointegrating vectors (last r columns)
  coint_vectors <- beta[, (n_vars - r + 1):n_vars, drop = FALSE]
  rownames(coint_vectors) <- colnames(price_matrix)
  colnames(coint_vectors) <- paste0("CV", 1:r)
  
  cat("\n=======================================================================\n")
  cat("COINTEGRATING VECTORS (β)\n")
  cat("=======================================================================\n\n")
  print(round(coint_vectors, 4))
  
  # Construct the stationary linear combination(s)
  cat("\n=======================================================================\n")
  cat("LINEAR COMBINATION FORMULA(S)\n")
  cat("=======================================================================\n\n")
  
  for (i in 1:r) {
    cat("Cointegrating Relationship", i, ":\n")
    cat("z_", i, "(t) = ", sep = "")
    
    terms <- character(n_vars)
    for (j in 1:n_vars) {
      coef <- coint_vectors[j, i]
      asset <- colnames(price_matrix)[j]
      
      if (j == 1) {
        terms[j] <- sprintf("%.4f * %s(t)", coef, asset)
      } else {
        sign <- ifelse(coef >= 0, "+", "")
        terms[j] <- sprintf(" %s %.4f * %s(t)", sign, coef, asset)
      }
    }
    
    cat(paste(terms, collapse = ""), "\n")
    
    # Compute and show statistics of the spread
    spread <- price_matrix %*% coint_vectors[, i]
    cat("  Mean:", round(mean(spread), 4), "\n")
    cat("  SD:  ", round(sd(spread), 4), "\n")
    cat("  ADF: ", round(ur.df(spread, type = "drift")@teststat[1], 4), 
        " (should be < -3 for stationarity)\n\n")
  }
}
```

## Role of the Intercept

### When `ecdet = "const"` (Default in the Code)

The Johansen test includes a constant term in the cointegrating relationship:

```
β' Y_t + c ~ I(0)
```

The constant `c` is **implicitly included** in the VECM but not explicitly in the eigenvector β.

### Extracting the Intercept

```r
# The intercept can be estimated from the mean of the cointegrating relationship
spread <- price_matrix %*% coint_vectors[, 1]
intercept <- -mean(spread)  # Negative because we want z_t - c to be mean-zero

# Adjusted formula with explicit intercept
cat("z(t) = β' Y(t) + ", round(intercept, 4), "\n")
```

### Alternative: No Intercept (`ecdet = "none"`)

If you specify `ecdet = "none"` in `ca.jo()`:

```r
johansen_test <- ca.jo(
  price_matrix,
  type = "trace",
  ecdet = "none",  # No constant term
  K = johansen_K,
  spec = "transitory"
)
```

Then the cointegrating relationship is:

```
β' Y_t ~ I(0)
```

With no intercept term, and the spread should fluctuate around zero.

## Trading Strategy Application

### Mean-Reversion Signal

```r
# Calculate the spread (stationary combination)
spread <- price_matrix %*% coint_vectors[, 1]

# Normalize to z-scores
spread_mean <- mean(spread)
spread_sd <- sd(spread)
z_score <- (spread - spread_mean) / spread_sd

# Trading signals
# Long spread when z_score < -2 (expect reversion up)
# Short spread when z_score > 2 (expect reversion down)

signals <- ifelse(z_score < -2, 1,
                 ifelse(z_score > 2, -1, 0))
```

### Weights for Portfolio

The cointegrating vector gives the relative weights:

```r
# Normalize weights to sum to 1 (for portfolio allocation)
weights <- coint_vectors[, 1]
weights_normalized <- weights / sum(abs(weights))

cat("Portfolio Weights:\n")
for (i in 1:length(weights_normalized)) {
  cat(sprintf("  %s: %.4f\n", colnames(price_matrix)[i], weights_normalized[i]))
}
```

## Complete Example Function

```r
#' Extract and Display Cointegrating Relationships
#'
#' @param result Output from test_cointegration_rank()
#' @param price_matrix The original price matrix used in the test
#' @return List containing cointegrating vectors and spreads
extract_cointegrating_relationships <- function(result, price_matrix) {
  if (!result$valid_for_coint || result$coint_rank == 0) {
    stop("No cointegrating relationships found")
  }
  
  johansen <- result$johansen
  r <- result$coint_rank
  n_vars <- ncol(price_matrix)
  
  # Extract cointegrating vectors
  beta <- johansen@V
  coint_vectors <- beta[, (n_vars - r + 1):n_vars, drop = FALSE]
  rownames(coint_vectors) <- colnames(price_matrix)
  colnames(coint_vectors) <- paste0("CV", 1:r)
  
  # Calculate spreads
  spreads <- price_matrix %*% coint_vectors
  colnames(spreads) <- paste0("Spread_", 1:r)
  
  # Calculate statistics for each spread
  spread_stats <- data.frame(
    relationship = 1:r,
    mean = apply(spreads, 2, mean),
    sd = apply(spreads, 2, sd),
    adf_stat = sapply(1:r, function(i) {
      ur.df(spreads[, i], type = "drift")@teststat[1]
    }),
    stringsAsFactors = FALSE
  )
  
  list(
    coint_vectors = coint_vectors,
    spreads = spreads,
    spread_stats = spread_stats
  )
}
```

## Summary

1. **Linear Combination Formula**: `z_t = β₁ X_t + β₂ Y_t + ... + βₙ Xₙ_t + c`

2. **Extract from Johansen**: Use `johansen@V` to get eigenvectors; the last `r` columns are cointegrating vectors

3. **Intercept**: 
   - With `ecdet = "const"`: intercept implicitly included, estimate from spread mean
   - With `ecdet = "none"`: no intercept, spread oscillates around zero

4. **Interpretation**: Each cointegrating vector defines a stationary linear combination that can be used for mean-reversion trading

5. **Trading**: Construct spread, normalize to z-scores, and trade when spread deviates significantly from mean
