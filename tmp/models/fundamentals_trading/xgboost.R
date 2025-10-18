devtools::load_all()
library(xgboost)
library(caret)

set.seed(123)

fetl <- Fetl$new()

# Load and prepare data
sfm <- fetl$send_query("SELECT * FROM tmp_sfm") %>%
  tibble %>%
  dplyr::filter(
    exchange %in% c("NASDAQ", "NYSE")
  ) %>%
  dplyr::mutate(
    # upside_type = ifelse(upside > log(1.6), "strike", "lost")
    downside_type = ifelse(downside < log(0.7), "strike", "lost")
  ) %>%
  dplyr::select(
    -symbol,
    -ipo_date,
    -report_date,
    -starts_with("is_date"),
    -downside,
    -upside,
    -exchange,
    -sector,
    -industry
  )

# # Train/test split (upside)
# idx <- createDataPartition(sfm$upside_type, p = 0.4, list = FALSE)
# train_x <- data.matrix(sfm[idx, -which(names(sfm) == "upside_type")])
# train_y <- as.numeric(factor(sfm$upside_type[idx])) - 1  # Convert to 0/1 for xgboost
# test_x <- data.matrix(sfm[-idx, -which(names(sfm) == "upside_type")])
# test_y <- as.numeric(factor(sfm$upside_type[-idx])) - 1
# test_y_labels <- sfm$upside_type[-idx]

# Train/test split (downside)
idx <- createDataPartition(sfm$downside_type, p = 0.7, list = FALSE)
train_x <- data.matrix(sfm[idx, -which(names(sfm) == "downside_type")])
train_y <- as.numeric(factor(sfm$downside_type[idx])) - 1  # Convert to 0/1 for xgboost
test_x <- data.matrix(sfm[-idx, -which(names(sfm) == "downside_type")])
test_y <- as.numeric(factor(sfm$downside_type[-idx])) - 1
test_y_labels <- sfm$downside_type[-idx]

# Train XGBoost classification model
dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest <- xgb.DMatrix(data = test_x, label = test_y)

params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  scale_pos_weight = 0.7  # < 1 makes model more aggressive in predicting strikes
)

model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain, test = dtest),
  verbose = 0
)

# Predictions
pred_probs <- predict(model, dtest)

# === THRESHOLD OPTIMIZATION FOR MAXIMIZING RECALL ===
cat("\n=== THRESHOLD OPTIMIZATION (Maximizing Strike Detection) ===\n")
thresholds <- seq(0.3, 0.7, by = 0.05)
results <- data.frame()

for (thresh in thresholds) {
  pred_class_tmp <- ifelse(pred_probs > thresh, 1, 0)
  cm_tmp <- table(Predicted = pred_class_tmp, Actual = test_y)

  tp <- cm_tmp[2, 2]
  tn <- cm_tmp[1, 1]
  fp <- cm_tmp[2, 1]
  fn <- cm_tmp[1, 2]

  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1 <- 2 * (precision * recall) / (precision + recall)
  accuracy <- (tp + tn) / sum(cm_tmp)

  results <- rbind(results, data.frame(
    threshold = thresh,
    precision = precision,
    recall = recall,
    f1 = f1,
    accuracy = accuracy,
    tp = tp,
    fp = fp,
    fn = fn,
    tn = tn
  ))
}

print(results)

# Find optimal threshold for maximum recall
optimal_idx <- which.max(results$recall)
optimal_threshold <- results$threshold[optimal_idx]
cat(sprintf("\n*** OPTIMAL THRESHOLD FOR MAX RECALL: %.2f ***\n", optimal_threshold))
cat(sprintf("At this threshold: Recall=%.4f, Precision=%.4f, F1=%.4f\n\n",
            results$recall[optimal_idx],
            results$precision[optimal_idx],
            results$f1[optimal_idx]))

# Use optimal threshold for final predictions
pred_class <- ifelse(pred_probs > optimal_threshold, 1, 0)

# === CLASSIFICATION METRICS ===
cat("\n=== CLASSIFICATION METRICS ===\n")
accuracy <- mean(pred_class == test_y)
cat(sprintf("Accuracy: %.4f\n", accuracy))

# Confusion Matrix Stats
cm <- table(Predicted = pred_class, Actual = test_y)
cat("\nConfusion Matrix:\n")
print(cm)

# Detailed Confusion Matrix
cat("\n=== DETAILED CONFUSION MATRIX ===\n")
cat(sprintf("True Negatives (TN):  %d - Correctly predicted 'lost'\n", cm[1, 1]))
cat(sprintf("False Positives (FP): %d - Incorrectly predicted 'strike' (was 'lost')\n", cm[2, 1]))
cat(sprintf("False Negatives (FN): %d - Incorrectly predicted 'lost' (was 'strike')\n", cm[1, 2]))
cat(sprintf("True Positives (TP):  %d - Correctly predicted 'strike'\n", cm[2, 2]))
cat(sprintf("\nTotal samples: %d\n", sum(cm)))
cat(sprintf("Correctly classified: %d (%.2f%%)\n", cm[1,1] + cm[2,2], 100 * (cm[1,1] + cm[2,2]) / sum(cm)))
cat(sprintf("Misclassified: %d (%.2f%%)\n\n", cm[2,1] + cm[1,2], 100 * (cm[2,1] + cm[1,2]) / sum(cm)))

# Additional metrics
library(pROC)
roc_obj <- roc(test_y, pred_probs, quiet = TRUE)
cat(sprintf("AUC: %.4f\n", auc(roc_obj)))

# Precision, Recall, F1-Score
tp <- cm[2, 2]
tn <- cm[1, 1]
fp <- cm[2, 1]
fn <- cm[1, 2]

precision <- tp / (tp + fp)
recall <- tp / (tp + fn)
f1 <- 2 * (precision * recall) / (precision + recall)

cat(sprintf("Precision: %.4f\n", precision))
cat(sprintf("Recall: %.4f\n", recall))
cat(sprintf("F1-Score: %.4f\n", f1))

# === CONFUSION MATRIX PLOT ===
cm_df <- as.data.frame(cm)
cm_df$Predicted <- factor(cm_df$Predicted, levels = c(0, 1), labels = c("lost", "strike"))
cm_df$Actual <- factor(cm_df$Actual, levels = c(0, 1), labels = c("lost", "strike"))

p_cm <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 8, color = "white") +
  scale_fill_gradient(low = "#2c3e50", high = "#e74c3c") +
  theme_minimal() +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
  theme(axis.text = element_text(size = 12))
print(p_cm)

# === ROC CURVE ===
roc_df <- data.frame(
  TPR = roc_obj$sensitivities,
  FPR = 1 - roc_obj$specificities
)

p_roc <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "#3498db", linewidth = 1.5) +
  geom_abline(linetype = "dashed", color = "gray") +
  theme_minimal() +
  labs(
    title = sprintf("ROC Curve (AUC = %.4f)", auc(roc_obj)),
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme(plot.title = element_text(hjust = 0.5))
print(p_roc)

# === FEATURE IMPORTANCE ===
imp <- xgb.importance(model = model)
xgb.plot.importance(imp[1:10,])

# === PROBABILITY DISTRIBUTION ===
pred_df <- data.frame(
  Probability = pred_probs,
  Actual = factor(test_y, levels = c(0, 1), labels = c("lost", "strike"))
)

p_dist <- ggplot(pred_df, aes(x = Probability, fill = Actual)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Predicted Probability Distribution",
    x = "Predicted Probability",
    y = "Count"
  ) +
  scale_fill_manual(values = c("lost" = "#e74c3c", "strike" = "#2ecc71"))
print(p_dist)

# === CLASS DISTRIBUTION ===
cat("\n=== CLASS DISTRIBUTION ===\n")
cat("Training set:\n")
print(table(factor(train_y, levels = c(0, 1), labels = c("lost", "strike"))))
cat("\nTest set:\n")
print(table(factor(test_y, levels = c(0, 1), labels = c("lost", "strike"))))
