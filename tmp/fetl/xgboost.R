devtools::load_all()
library(xgboost)
library(caret)
library(ggplot2)

fetl <- Fetl$new()

# Load and prepare data
sfm <- fetl$send_query("SELECT * FROM tmp_sfm") %>%
  tibble() %>%
  filter(
    upside <= 1.5,
    sector == 'Financial Services',
    exchange == 'NASDAQ'
  ) %>%
  mutate(upside = upside - 1) %>%
  select(-symbol, -ipo_date, -date_ready, -starts_with("is_date"), -downside, -exchange, -sector, -industry) %>%
  # na.omit() %>%
  as.data.frame() %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

# Train/test split
set.seed(123)
idx <- createDataPartition(sfm$upside, p = 0.8, list = FALSE)
train_x <- data.matrix(sfm[idx, -which(names(sfm) == "upside")])
train_y <- sfm$upside[idx]
test_x <- data.matrix(sfm[-idx, -which(names(sfm) == "upside")])
test_y <- sfm$upside[-idx]

# Train model
model <- xgboost(data = train_x, label = train_y, nrounds = 100, max_depth = 6, eta = 0.1, verbose = 0)
pred <- predict(model, test_x)

# Results
cat(sprintf("RMSE: %.4f | MAE: %.4f | R²: %.4f\n", sqrt(mean((test_y - pred)^2)), mean(abs(test_y - pred)), cor(test_y, pred)^2))

# Charts
imp <- xgb.importance(model = model); xgb.plot.importance(imp[1:10,])
df <- data.frame(Actual = test_y, Predicted = pred)
print(ggplot(df, aes(Actual, Predicted)) + geom_point(alpha = 0.5) + geom_abline(color = "red") + theme_minimal() + labs(title = "Predictions vs Actual"))


# Results
r <- cbind(pred, test_y) %>% tibble

r %>%
  filter(
    pred > 0.2
  ) %>%
  summarise(
    min = min(test_y),
    mean = mean(test_y),
    sd = sd(test_y),
    max = max(test_y),
    q25 = quantile(test_y, 0.25),
    q05 = quantile(test_y, 0.05),
    n = n()
  )


sfm %>% summarise(
  min = min(upside),
  mean = mean(upside),
  sd = sd(upside),
  max = max(upside),
  q25 = quantile(upside, 0.25),
  q05 = quantile(upside, 0.05),
  n = n()
)
