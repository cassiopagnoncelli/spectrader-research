suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(VGAM)
  library(glmnet)
  library(ggplot2)
})

############################################################
# 0. Build df_train from X and ys
############################################################

df_train <- as.data.table(X[train_indices])

df_train[, y          := ys$excursion_high[train_indices]]
df_train[, y_kurtosis := ys$kurtosis[train_indices]]
df_train[, y_skewness := ys$skewness[train_indices]]

df_train <- as.data.frame(df_train)
df_train <- df_train |> drop_na(y)

############################################################
# 1. Threshold and exceedances
############################################################

u <- quantile(df_train$y, 0.975, na.rm = TRUE)

df_ex <- df_train[df_train$y > u, , drop = FALSE]
df_ex$excess <- df_ex$y - u

############################################################
# 2. Clean predictor matrix (numeric, no NAs, no zero-var)
############################################################

num_cols  <- names(df_ex)[sapply(df_ex, is.numeric)]
drop_cols <- c("y", "y_kurtosis", "y_skewness", "excess")
pred_cols <- setdiff(num_cols, drop_cols)

df_evt <- df_ex[, c("excess", pred_cols), drop = FALSE]
df_evt <- df_evt |> drop_na()

# zero-variance filter
sds <- sapply(df_evt[, pred_cols, drop = FALSE], sd)
pred_cols <- pred_cols[sds > 0]

# limit dimensionality
if (length(pred_cols) > 30) {
  pred_cols <- pred_cols[1:30]
}
df_evt <- df_evt[, c("excess", pred_cols), drop = FALSE]

############################################################
# 3. Variable selection via glmnet
############################################################

X_mat    <- as.matrix(df_evt[, pred_cols, drop = FALSE])
y_excess <- df_evt$excess

set.seed(123)
cv_fit <- cv.glmnet(
  x      = X_mat,
  y      = y_excess,
  alpha  = 0.5,
  family = "gaussian"
)

coef_vec <- coef(cv_fit, s = "lambda.min")
idx      <- which(coef_vec != 0)[-1]  # drop intercept

if (length(idx) == 0) {
  sel_vars <- pred_cols[1:min(5, length(pred_cols))]
} else {
  sel_vars <- pred_cols[idx]
}

############################################################
# 4. Fit EVT (GPD) model with VGAM
############################################################

evt_formula <- as.formula(
  paste("excess ~", paste(sel_vars, collapse = " + "))
)

fit_evt <- vglm(
  formula = evt_formula,
  family  = gpd(),
  data    = df_evt
)

print(summary(fit_evt))

pred_evt <- predict(fit_evt, type = "response")
sigma_hat <- pred_evt[, 1]
xi_hat    <- pred_evt[, 2]

df_evt$sigma_hat <- sigma_hat
df_evt$xi_hat    <- xi_hat

############################################################
# 5. PIT and QQ diagnostics
############################################################

df_evt$pit <- (1 + df_evt$xi_hat * df_evt$excess / df_evt$sigma_hat) ^ (-1 / df_evt$xi_hat)

ggplot(df_evt, aes(pit)) +
  geom_histogram(bins = 40) +
  theme_minimal() +
  labs(title = "EVT PIT Histogram", x = "PIT", y = "Count")

gpd_quantile_excess <- function(alpha, sigma, xi) {
  (sigma / xi) * ((1 - alpha)^(-xi) - 1)
}

df_evt$theoretical <- gpd_quantile_excess(
  alpha = rank(df_evt$excess) / (nrow(df_evt) + 1),
  sigma = df_evt$sigma_hat,
  xi    = df_evt$xi_hat
)

ggplot(df_evt, aes(x = theoretical, y = excess)) +
  geom_point(alpha = 0.5) +
  geom_abline(color = "red") +
  theme_minimal() +
  labs(title = "EVT QQ Plot", x = "Theoretical excess", y = "Observed excess")

############################################################
# 6. EVT tail metrics and score
############################################################

gpd_tail_prob <- function(y_star, u, sigma, xi) {
  z <- 1 + xi * (y_star - u) / sigma
  z <- pmax(z, 1e-12)
  z ^ (-1 / xi)
}

gpd_quantile <- function(alpha, u, sigma, xi) {
  u + (sigma / xi) * ((1 - alpha)^(-xi) - 1)
}

df_evt$p_y_gt_1_3 <- gpd_tail_prob(1.3, u, df_evt$sigma_hat, df_evt$xi_hat)
df_evt$q95        <- gpd_quantile(0.95, u, df_evt$sigma_hat, df_evt$xi_hat)
df_evt$q99        <- gpd_quantile(0.99, u, df_evt$sigma_hat, df_evt$xi_hat)

df_evt$E_excess <- df_evt$sigma_hat / pmax(1 - df_evt$xi_hat, 1e-9)
df_evt$E_y      <- u + df_evt$E_excess

df_evt$evt_score   <- df_evt$p_y_gt_1_3 * df_evt$q99 * df_evt$E_y
df_evt$evt_score_z <- as.numeric(scale(df_evt$evt_score))

df_evt
