if (FALSE) {
  devtools::load_all()

  source("rd/models/stock_crossover/1_etl.R")
  source("rd/models/stock_crossover/2_feature_engineering.R")
  source("rd/models/stock_crossover/3_splits.R")
  source("rd/models/stock_crossover/4_feature_enrichment.R")
  source("rd/models/stock_crossover/5_datasets.R")
}

# SIGNAL RESEARCH.
# Research on entry signals.
#
# Output: Intermediate step for signal models.

### Extreme-Tail Quantile Modeling (qboost-only)
y <- Y$excursion_high

tail_idx <- base::intersect(which(y > stats::quantile(y[train_idx], 0.99)), train_idx)

pca <- stats::prcomp(nX[tail_idx, ], center = TRUE, scale. = TRUE)

PC_train <- stats::predict(pca, nX[train_idx, ])[, 1:30]
PC_val <- stats::predict(pca, nX[val_idx, ])[, 1:30]
PC_test <- stats::predict(pca, nX[test_idx, ])[, 1:30]

make_pc_nonlinear <- function(PC) {
  base::data.frame(
    PC1_sq  = PC[, 1]^2,
    PC3_sq  = PC[, 3]^2,
    PC9_sq  = PC[, 9]^2,
    abs_PC1 = base::abs(PC[, 1]),
    abs_PC3 = base::abs(PC[, 3]),
    abs_PC9 = base::abs(PC[, 9]),
    PC1_PC3 = PC[, 1] * PC[, 3],
    PC3_PC9 = PC[, 3] * PC[, 9],
    PC1_PC9 = PC[, 1] * PC[, 9]
  )
}

NL_train <- make_pc_nonlinear(PC_train)
NL_val <- make_pc_nonlinear(PC_val)
NL_test <- make_pc_nonlinear(PC_test)

X_train <- base::cbind.data.frame(nX[train_idx, ], PC_train, NL_train)
X_val <- base::cbind.data.frame(nX[val_idx, ], PC_val, NL_val)
X_test <- base::cbind.data.frame(nX[test_idx, ], PC_test, NL_test)

taus <- c(0.99, 0.999)

qboost_models <- base::lapply(taus, function(tau) {
  qboost::qboost(
    x = X_train,
    y = y[train_idx],
    tau = tau,
    params = base::list(
      num_leaves       = 96,
      learning_rate    = 0.05,
      max_depth        = 10,
      min_data_in_leaf = 20,
      feature_fraction = 0.9,
      bagging_fraction = 0.9,
      bagging_freq     = 1
    )
  )
})

base::names(qboost_models) <- base::paste0("q", taus)

pred_val <- base::sapply(qboost_models, stats::predict, newdata = X_val)
pred_test <- base::sapply(qboost_models, stats::predict, newdata = X_test)

signal_val <- pred_val[, "q0.999"]
signal_test <- pred_test[, "q0.999"]

rank_val <- base::order(signal_val, decreasing = TRUE)
rank_test <- base::order(signal_test, decreasing = TRUE)

top_val <- rank_val[1:500]
top_test <- rank_test[1:500]

idx_val <- base::which(signal_val > stats::quantile(signal_val, 0.999))
idx_test <- base::which(signal_test > stats::quantile(signal_test, 0.999))

kendall_val <- stats::cor(y[val_idx][idx_val], signal_val[idx_val], method = "kendall")
kendall_test <- stats::cor(y[test_idx][idx_test], signal_test[idx_test], method = "kendall")

results <- base::list(
  pca          = pca,
  models       = qboost_models,
  signal_val   = signal_val,
  signal_test  = signal_test,
  top_val      = top_val,
  top_test     = top_test,
  kendall_val  = kendall_val,
  kendall_test = kendall_test
)

message("Kendall Tau (Validation): ", round(kendall_val, 4))
message("Kendall Tau (Test): ", round(kendall_test, 4))
