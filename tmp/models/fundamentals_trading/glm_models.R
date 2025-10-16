devtools::load_all()
library(glmnet)
library(caret)

fetl <- Fetl$new()

sfm <- fetl$send_query("SELECT * FROM tmp_sfm") %>%
  tibble() %>%
  select(
    -symbol,
    -ipo_date,
    -date_ready,
    -is_date_0,
    -is_date_1,
    -is_date_2,
    -is_date_3,
    -is_date_4,
    -is_date_5,
    -downside
  )

# Prepare data
# Separate target variable (y) and features (X)
y <- sfm$upside
X <- sfm %>% select(-upside)

# One-hot encode categorical variables
categorical_vars <- c("exchange", "sector", "industry")
dummies <- dummyVars(~ exchange + sector + industry, data = X)
X_encoded <- predict(dummies, newdata = X) %>% as.data.frame()

# Combine with numeric features
numeric_features <- X %>% select(-all_of(categorical_vars))
X_final <- cbind(numeric_features, X_encoded)

# Remove rows with NA in target variable
valid_idx <- !is.na(y)
X_final <- X_final[valid_idx, ]
y <- y[valid_idx]

# Handle missing values in features (impute with median)
for (col in names(X_final)) {
  if (any(is.na(X_final[[col]]))) {
    X_final[[col]][is.na(X_final[[col]])] <- median(X_final[[col]], na.rm = TRUE)
  }
}

# Split data into train and test sets (80/20)
set.seed(42)
train_idx <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X_final[train_idx, ]
X_test <- X_final[-train_idx, ]
y_train <- y[train_idx]
y_test <- y[-train_idx]

# Standardize features (required for glmnet)
preproc <- preProcess(X_train, method = c("center", "scale"))
X_train_scaled <- predict(preproc, X_train)
X_test_scaled <- predict(preproc, X_test)

# Convert to matrices for glmnet
X_train_matrix <- as.matrix(X_train_scaled)
X_test_matrix <- as.matrix(X_test_scaled)

cat(sprintf("\nDataset sizes:\n"))
cat(sprintf("Training: %d samples\n", nrow(X_train_matrix)))
cat(sprintf("Test: %d samples\n", nrow(X_test_matrix)))
cat(sprintf("Features: %d\n", ncol(X_train_matrix)))

# Print feature names to verify all are included
cat("\nFeature names (first 20):\n")
print(head(colnames(X_train_matrix), 20))
cat(sprintf("\n...and %d more features\n\n", max(0, ncol(X_train_matrix) - 20)))

# Store results
results <- list()

# ============================================================
# 0. UNREGULARIZED LINEAR MODEL (baseline)
# ============================================================
cat("\n=== Unregularized Linear Model (OLS) ===\n")

# Create a simple linear model with all features
# Combine X and y for lm
train_data <- as.data.frame(X_train_scaled)
train_data$y <- y_train

# Fit model
lm_model <- lm(y ~ ., data = train_data)

# Predictions
test_data <- as.data.frame(X_test_scaled)
lm_pred <- predict(lm_model, newdata = test_data)

# Evaluate
lm_rmse <- sqrt(mean((lm_pred - y_test)^2))
lm_mae <- mean(abs(lm_pred - y_test))
lm_r2 <- cor(lm_pred, y_test)^2

cat(sprintf("RMSE: %.4f\n", lm_rmse))
cat(sprintf("MAE: %.4f\n", lm_mae))
cat(sprintf("R-squared: %.4f\n", lm_r2))

# Count significant features
lm_summary <- summary(lm_model)
significant_features <- sum(lm_summary$coefficients[, 4] < 0.05) - 1  # Exclude intercept
cat(sprintf("Significant features (p < 0.05): %d / %d\n", significant_features, ncol(X_train_matrix)))

results$lm <- list(
  rmse = lm_rmse,
  mae = lm_mae,
  r2 = lm_r2,
  predictions = lm_pred,
  n_significant = significant_features
)

# ============================================================
# 1. RIDGE REGRESSION (alpha = 0)
# ============================================================
cat("\n=== Ridge Regression (L2 Regularization) ===\n")

# Cross-validation to find optimal lambda
cv_ridge <- cv.glmnet(
  X_train_matrix, 
  y_train,
  alpha = 0,  # Ridge
  nfolds = 10,
  type.measure = "mse"
)

# Plot cross-validation results
plot(cv_ridge, main = "Ridge Regression: CV Error vs Lambda")

# Best lambda
cat(sprintf("Optimal lambda: %.4f\n", cv_ridge$lambda.min))

# Train with optimal lambda
ridge_model <- glmnet(
  X_train_matrix,
  y_train,
  alpha = 0,
  lambda = cv_ridge$lambda.min
)

# Predictions
ridge_pred <- predict(ridge_model, newx = X_test_matrix)
ridge_pred <- as.vector(ridge_pred)

# Evaluate
ridge_rmse <- sqrt(mean((ridge_pred - y_test)^2))
ridge_mae <- mean(abs(ridge_pred - y_test))
ridge_r2 <- cor(ridge_pred, y_test)^2

cat(sprintf("RMSE: %.4f\n", ridge_rmse))
cat(sprintf("MAE: %.4f\n", ridge_mae))
cat(sprintf("R-squared: %.4f\n", ridge_r2))

results$ridge <- list(
  rmse = ridge_rmse,
  mae = ridge_mae,
  r2 = ridge_r2,
  predictions = ridge_pred
)

# ============================================================
# 2. LASSO REGRESSION (alpha = 1)
# ============================================================
cat("\n=== Lasso Regression (L1 Regularization) ===\n")

# Cross-validation to find optimal lambda
cv_lasso <- cv.glmnet(
  X_train_matrix,
  y_train,
  alpha = 1,  # Lasso
  nfolds = 10,
  type.measure = "mse"
)

# Plot cross-validation results
plot(cv_lasso, main = "Lasso Regression: CV Error vs Lambda")

# Best lambda
cat(sprintf("Optimal lambda: %.4f\n", cv_lasso$lambda.min))

# Train with optimal lambda
lasso_model <- glmnet(
  X_train_matrix,
  y_train,
  alpha = 1,
  lambda = cv_lasso$lambda.min
)

# Predictions
lasso_pred <- predict(lasso_model, newx = X_test_matrix)
lasso_pred <- as.vector(lasso_pred)

# Evaluate
lasso_rmse <- sqrt(mean((lasso_pred - y_test)^2))
lasso_mae <- mean(abs(lasso_pred - y_test))
lasso_r2 <- cor(lasso_pred, y_test)^2

cat(sprintf("RMSE: %.4f\n", lasso_rmse))
cat(sprintf("MAE: %.4f\n", lasso_mae))
cat(sprintf("R-squared: %.4f\n", lasso_r2))

# Feature selection - count non-zero coefficients
lasso_coefs <- coef(lasso_model)
non_zero_coefs <- sum(lasso_coefs != 0) - 1  # Exclude intercept
cat(sprintf("Non-zero features: %d / %d\n", non_zero_coefs, ncol(X_train_matrix)))

results$lasso <- list(
  rmse = lasso_rmse,
  mae = lasso_mae,
  r2 = lasso_r2,
  predictions = lasso_pred,
  n_features = non_zero_coefs
)

# ============================================================
# 3. ELASTIC NET (alpha = 0.5)
# ============================================================
cat("\n=== Elastic Net (L1 + L2 Regularization) ===\n")

# Cross-validation to find optimal lambda
cv_elastic <- cv.glmnet(
  X_train_matrix,
  y_train,
  alpha = 0.5,  # Elastic Net
  nfolds = 10,
  type.measure = "mse"
)

# Plot cross-validation results
plot(cv_elastic, main = "Elastic Net: CV Error vs Lambda")

# Best lambda
cat(sprintf("Optimal lambda: %.4f\n", cv_elastic$lambda.min))

# Train with optimal lambda
elastic_model <- glmnet(
  X_train_matrix,
  y_train,
  alpha = 0.5,
  lambda = cv_elastic$lambda.min
)

# Predictions
elastic_pred <- predict(elastic_model, newx = X_test_matrix)
elastic_pred <- as.vector(elastic_pred)

# Evaluate
elastic_rmse <- sqrt(mean((elastic_pred - y_test)^2))
elastic_mae <- mean(abs(elastic_pred - y_test))
elastic_r2 <- cor(elastic_pred, y_test)^2

cat(sprintf("RMSE: %.4f\n", elastic_rmse))
cat(sprintf("MAE: %.4f\n", elastic_mae))
cat(sprintf("R-squared: %.4f\n", elastic_r2))

# Feature selection
elastic_coefs <- coef(elastic_model)
non_zero_coefs_elastic <- sum(elastic_coefs != 0) - 1
cat(sprintf("Non-zero features: %d / %d\n", non_zero_coefs_elastic, ncol(X_train_matrix)))

results$elastic <- list(
  rmse = elastic_rmse,
  mae = elastic_mae,
  r2 = elastic_r2,
  predictions = elastic_pred,
  n_features = non_zero_coefs_elastic
)

# ============================================================
# 4. COMPARISON WITH PREVIOUS MODELS
# ============================================================
cat("\n\n=== MODEL COMPARISON ===\n")
cat("Model          RMSE      MAE       R²       Notes\n")
cat("----------------------------------------------------------------\n")
cat(sprintf("XGBoost      233.29    136.48    0.1967   Baseline\n"))
cat(sprintf("Neural Net   359.07    157.05    0.0337   Poor\n"))
cat(sprintf("OLS          %.2f    %.2f    %.4f   All features (%d sig)\n", 
            lm_rmse, lm_mae, lm_r2, significant_features))
cat(sprintf("Ridge        %.2f    %.2f    %.4f   L2 regularization\n", 
            ridge_rmse, ridge_mae, ridge_r2))
cat(sprintf("Lasso        %.2f    %.2f    %.4f   L1 (%d features)\n", 
            lasso_rmse, lasso_mae, lasso_r2, non_zero_coefs))
cat(sprintf("Elastic Net  %.2f    %.2f    %.4f   L1+L2 (%d features)\n", 
            elastic_rmse, elastic_mae, elastic_r2, non_zero_coefs_elastic))

# Find best model overall
all_r2 <- c(lm_r2, ridge_r2, lasso_r2, elastic_r2)
best_r2 <- max(all_r2)
best_model <- c("OLS", "Ridge", "Lasso", "Elastic Net")[which.max(all_r2)]
cat(sprintf("\nBest model: %s (R² = %.4f)\n", best_model, best_r2))

if (best_r2 > 0.1967) {
  improvement <- ((best_r2 - 0.1967) / 0.1967) * 100
  cat(sprintf("✓ Improved over XGBoost by %.1f%%\n", improvement))
} else {
  decline <- ((0.1967 - best_r2) / 0.1967) * 100
  cat(sprintf("✗ Below XGBoost by %.1f%%\n", decline))
}

# ============================================================
# 5. FEATURE IMPORTANCE (for Lasso)
# ============================================================
cat("\n=== Top 20 Most Important Features (Lasso) ===\n")
lasso_coefs_df <- data.frame(
  Feature = rownames(lasso_coefs)[-1],  # Exclude intercept
  Coefficient = as.vector(lasso_coefs)[-1]
) %>%
  filter(Coefficient != 0) %>%
  mutate(AbsCoef = abs(Coefficient)) %>%
  arrange(desc(AbsCoef))

print(head(lasso_coefs_df, 20))

# ============================================================
# 6. VISUALIZATIONS
# ============================================================

# Predictions vs Actual for best model
if (best_model == "Ridge") {
  best_pred <- ridge_pred
} else if (best_model == "Lasso") {
  best_pred <- lasso_pred
} else {
  best_pred <- elastic_pred
}

par(mfrow = c(2, 2))

# Plot 1: Predictions vs Actual (Best GLM)
plot(y_test, best_pred,
     xlab = "Actual Upside",
     ylab = "Predicted Upside",
     main = sprintf("%s: Predictions vs Actual", best_model),
     pch = 16, col = rgb(0, 0, 1, 0.3))
abline(0, 1, col = "red", lwd = 2)
grid()

# Plot 2: Residuals (Best GLM)
residuals <- y_test - best_pred
plot(best_pred, residuals,
     xlab = "Predicted Upside",
     ylab = "Residuals",
     main = sprintf("%s: Residual Plot", best_model),
     pch = 16, col = rgb(0, 0, 1, 0.3))
abline(h = 0, col = "red", lwd = 2)
grid()

# Plot 3: Model Comparison
barplot(c(0.1967, 0.0337, ridge_r2, lasso_r2, elastic_r2),
        names.arg = c("XGBoost", "Neural Net", "Ridge", "Lasso", "Elastic"),
        ylab = "R-squared",
        main = "Model Performance Comparison",
        col = c("green", "red", "lightblue", "lightblue", "lightblue"),
        ylim = c(0, max(0.25, ridge_r2, lasso_r2, elastic_r2)))
abline(h = 0.1967, col = "darkgreen", lty = 2, lwd = 2)
text(1, 0.1967 + 0.01, "XGBoost baseline", pos = 3, cex = 0.8)
grid()

# Plot 4: Feature coefficients (top 15 from best model)
if (best_model == "Lasso" || best_model == "Elastic Net") {
  top_features <- head(lasso_coefs_df, 15)
  par(mar = c(5, 10, 4, 2))
  barplot(top_features$Coefficient,
          names.arg = top_features$Feature,
          horiz = TRUE,
          las = 1,
          main = sprintf("Top 15 Features (%s)", best_model),
          xlab = "Coefficient",
          col = ifelse(top_features$Coefficient > 0, "darkgreen", "darkred"))
  abline(v = 0, lty = 2)
}

par(mfrow = c(1, 1))

cat("\n=== Analysis Complete ===\n")
