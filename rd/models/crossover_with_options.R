devtools::load_all()

options(scipen = 999)

fetl <- Fetl$new()

# Load and prepare data
dfm_raw <- fetl$send_query("
    SELECT
      c.symbol,
      q.close,
      differential_forward_mass(c.symbol, q.date, 15) AS dfm_0
    FROM quotes q
    JOIN (
      SELECT
        c1.id,
        MAX(c1.symbol) AS symbol,
        COUNT(*) as num_quotes
      FROM quotes q1
      JOIN companies c1 ON q1.company_id = c1.id
      WHERE
        c1.is_actively_trading AND NOT c1.is_etf AND NOT c1.is_fund AND
        c1.symbol NOT LIKE '%-%' AND
        c1.exchange IN ('NYSE', 'NASDAQ') AND
        q1.close >= 5
      GROUP BY c1.id
      ORDER BY COUNT(*) DESC
      LIMIT 300
    ) c ON q.company_id = c.id
    INNER JOIN (
      SELECT
        mc.company_id,
        MAX(mc.date) AS date
      FROM market_caps mc
      GROUP BY mc.company_id
      ORDER BY mc.company_id
      LIMIT 1000
    ) mc ON mc.company_id = c.id
    WHERE
      q.date BETWEEN '2021-01-01' AND '2024-12-31'
  ") %>%
  tibble

dfm <- dfm_raw %>%
  group_by(symbol) %>%
  # Preprocessing
  dplyr::mutate(
    rsi_0 = TTR::RSI(close, n = 14),
    rsi_1 = lag(TTR::RSI(close, n = 14)),
    rsi_2 = lag(TTR::RSI(close, n = 14), 2),
    vol_0 = TTR::runSD(close, n = 10),
    vol_1 = lag(TTR::runSD(close, n = 10)),
    vol_2 = lag(TTR::runSD(close, n = 10), 2),
    sig_0 = TTR::SMA(close, n = 5),
    sig_1 = lag(TTR::SMA(close, n = 5)),
    sig_2 = lag(TTR::SMA(close, n = 5), 2),
    fast_0 = TTR::SMA(close, n = 20),
    fast_1 = lag(TTR::SMA(close, n = 20)),
    fast_2 = lag(TTR::SMA(close, n = 20), 2),
    slow_0 = TTR::SMA(close, n = 80),
    slow_1 = lag(TTR::SMA(close, n = 80)),
    slow_2 = lag(TTR::SMA(close, n = 80), 2)
  ) %>%
  # Calculated indicators
  mutate(
    rsi_vel = rsi_0 - rsi_1,
    rsi_accel = rsi_0 - 2 * rsi_1 + rsi_2,
    vol_vel = vol_0 - vol_1,
    vol_accel = vol_0 - 2 * vol_1 + vol_2,
    close_to_sig = close / sig_0,
    close_to_fast = close / fast_0,
    sig_fast_0 = sig_0 / fast_0,
    fast_slow_0 = fast_0 / slow_0,
    sig_vel = sig_0 - sig_1,
    sig_accel = sig_0 - 2 * sig_1 + sig_2,
    fast_vel = fast_0 - fast_1,
    fast_accel = fast_0 - 2 * fast_1 + fast_2,
    slow_vel = slow_0 - slow_1,
    slow_accel = slow_0 - 2 * slow_1 + slow_2
  ) %>%
  ungroup() %>%
  na.omit() %>%
  select(-c(
    symbol,
    close,
    vol_0, vol_1, vol_2,
    sig_0, sig_1, sig_2,
    fast_0, fast_1, fast_2,
    slow_0, slow_1, slow_2
  ))

dfm

# Model.

# Split data into train and test sets (80/20 split)
set.seed(42)
train_size <- floor(0.3 * nrow(dfm))
train_indices <- 1:train_size
test_indices <- (train_size + 1):nrow(dfm)

train_data <- dfm[train_indices, ]
test_data <- dfm[test_indices, ]

# Prepare feature matrix and target variable
feature_cols <- setdiff(names(dfm), "dfm_0")

train_x <- as.matrix(train_data[, feature_cols])
train_y <- train_data$dfm_0

test_x <- as.matrix(test_data[, feature_cols])
test_y <- test_data$dfm_0

# Create DMatrix for XGBoost
dtrain <- xgboost::xgb.DMatrix(data = train_x, label = train_y)
dtest <- xgboost::xgb.DMatrix(data = test_x, label = test_y)

# Set XGBoost parameters
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,              # learning rate
  max_depth = 6,          # maximum depth of trees
  subsample = 0.8,        # subsample ratio of training data
  colsample_bytree = 0.8, # subsample ratio of columns
  min_child_weight = 1,
  gamma = 0
)

# Train the model with cross-validation monitoring
watchlist <- list(train = dtrain, test = dtest)

xgb_model <- xgboost::xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,
  watchlist = watchlist,
  early_stopping_rounds = 50,
  verbose = 1
)

# Make predictions
train_pred <- predict(xgb_model, dtrain)
test_pred <- predict(xgb_model, dtest)

# Calculate performance metrics
train_rmse <- sqrt(mean((train_y - train_pred)^2))
test_rmse <- sqrt(mean((test_y - test_pred)^2))
train_mae <- mean(abs(train_y - train_pred))
test_mae <- mean(abs(test_y - test_pred))
train_r2 <- cor(train_y, train_pred)^2
test_r2 <- cor(test_y, test_pred)^2

cat("\n=== Model Performance ===\n")
cat(sprintf("Train RMSE: %.6f\n", train_rmse))
cat(sprintf("Test RMSE:  %.6f\n", test_rmse))
cat(sprintf("Train MAE:  %.6f\n", train_mae))
cat(sprintf("Test MAE:   %.6f\n", test_mae))
cat(sprintf("Train R²:   %.6f\n", train_r2))
cat(sprintf("Test R²:    %.6f\n", test_r2))

# Feature importance
importance_matrix <- xgboost::xgb.importance(
  feature_names = feature_cols,
  model = xgb_model
)
print(importance_matrix)

# Plot feature importance
xgboost::xgb.plot.importance(
  importance_matrix = importance_matrix,
  top_n = 10
)

# Create results dataframe
results <- data.frame(
  actual = test_y,
  predicted = test_pred,
  residual = test_y - test_pred
)

# Plot predictions vs actuals
plot(test_y, test_pred,
     main = "XGBoost: Predicted vs Actual dfm_0",
     xlab = "Actual dfm_0",
     ylab = "Predicted dfm_0",
     pch = 16,
     col = rgb(0, 0, 1, 0.5))
abline(0, 1, col = "red", lwd = 2)
grid()

# Plot residuals
plot(test_pred, results$residual,
     main = "Residual Plot",
     xlab = "Predicted dfm_0",
     ylab = "Residuals",
     pch = 16,
     col = rgb(0, 0, 1, 0.5))
abline(h = 0, col = "red", lwd = 2)
grid()
