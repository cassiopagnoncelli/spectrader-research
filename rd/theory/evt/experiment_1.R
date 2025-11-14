suppressPackageStartupMessages({
  library(data.table)
  library(VGAM)
  library(glmnet)
  library(dplyr)
  library(ggplot2)
})

# ------------------------------------------------------------
# 0. Data: [X | y]
# ------------------------------------------------------------
df_train <- copy(X[train_indices])
setDT(df_train)

df_train[, y          := ys$extreme_high_identity[train_indices]]
df_train[, y_kurtosis := ys$kurtosis[train_indices]]
df_train[, y_skewness := ys$skewness[train_indices]]

df_train <- df_train |> as.data.frame() |> tidyr::drop_na(y)

# ------------------------------------------------------------
# 1. Threshold and exceedances
# ------------------------------------------------------------
u_threshold <- quantile(df_train$y, 0.975, na.rm = TRUE)
u <- u_threshold

df_ex <- df_train[df_train$y > u, , drop = FALSE]
df_ex$excess <- df_ex$y - u

# ------------------------------------------------------------
# 2. Build clean modeling frame (no NAs)
# ------------------------------------------------------------
# numeric predictors only, drop obvious response columns
num_cols <- names(df_ex)[sapply(df_ex, is.numeric)]
drop_cols <- c("y", "y_kurtosis", "y_skewness", "excess")
pred_cols <- setdiff(num_cols, drop_cols)

df_model <- df_ex[, c("excess", pred_cols), drop = FALSE]
df_model <- tidyr::drop_na(df_model)   # <- removes all rows with any NA

X_mat <- as.matrix(df_model[, pred_cols, drop = FALSE])
y_excess <- df_model$excess

# ------------------------------------------------------------
# 3. Variable screening with glmnet (scale & rough shape proxy)
# ------------------------------------------------------------
set.seed(123)

# scale: predict excess
cv_scale <- cv.glmnet(
  x = X_mat,
  y = y_excess,
  alpha = 0.5,
  family = "gaussian"
)

nz_scale <- which(coef(cv_scale, s = "lambda.min")@i[-1] > 0)
scale_vars <- colnames(X_mat)[coef(cv_scale, s = "lambda.min")@i[-1]]
scale_vars <- unique(scale_vars)

# shape proxy: heavier emphasis on large excess
y_shape_proxy <- pmin(y_excess, quantile(y_excess, 0.9))

cv_shape <- cv.glmnet(
  x = X_mat,
  y = y_shape_proxy,
  alpha = 0.5,
  family = "gaussian"
)

shape_vars <- colnames(X_mat)[coef(cv_shape, s = "lambda.1se")@i[-1]]
shape_vars <- unique(shape_vars)
shape_vars <- head(shape_vars, 3)  # keep shape side very small

# final predictor set
evt_vars <- unique(c(scale_vars, shape_vars))

df_evt <- df_model[, c("excess", evt_vars), drop = FALSE]

# ------------------------------------------------------------
# 4. Fit GPD model with VGAM
# ------------------------------------------------------------
evt_formula <- as.formula(
  paste("excess ~", paste(evt_vars, collapse = " + "))
)

fit_vgam <- vglm(
  formula = evt_formula,
  family  = gpd(),
  data    = df_evt
)

summary(fit_vgam)

# ------------------------------------------------------------
# 5. Extract sigma, xi
# ------------------------------------------------------------
pred_evt <- predict(fit_vgam, type = "response")
sigma_hat <- pred_evt[, 1]
xi_hat    <- pred_evt[, 2]

df_evt$sigma_hat <- sigma_hat
df_evt$xi_hat    <- xi_hat

# ------------------------------------------------------------
# 6. EVT helpers: tail prob, quantile
# ------------------------------------------------------------
gpd_tail_prob <- function(y_star, u, sigma, xi) {
  z <- 1 + xi * (y_star - u) / sigma
  z <- pmax(z, 1e-12)
  z ^ (-1 / xi)
}

gpd_quantile <- function(alpha, u, sigma, xi) {
  u + (sigma / xi) * ((1 - alpha)^(-xi) - 1)
}

# ------------------------------------------------------------
# 7. PIT and QQ diagnostics
# ------------------------------------------------------------
df_evt$pit <- (1 + df_evt$xi_hat * df_evt$excess / df_evt$sigma_hat) ^
  (-1 / df_evt$xi_hat)

ggplot(df_evt, aes(pit)) +
  geom_histogram(bins = 40) +
  theme_minimal() +
  labs(title = "EVT PIT Histogram", x = "PIT", y = "count")

df_evt$theoretical <- gpd_quantile(
  alpha = rank(df_evt$excess) / (nrow(df_evt) + 1),
  u     = 0,
  sigma = df_evt$sigma_hat,
  xi    = df_evt$xi_hat
)

ggplot(df_evt, aes(x = theoretical, y = excess)) +
  geom_point(alpha = 0.5) +
  geom_abline(color = "red") +
  theme_minimal() +
  labs(title = "EVT QQ Plot", x = "Theoretical excess", y = "Observed excess")

# ------------------------------------------------------------
# 8. EVT scores
# ------------------------------------------------------------
df_evt$p_y_gt_1_3 <- gpd_tail_prob(1.3, u, df_evt$sigma_hat, df_evt$xi_hat)
df_evt$q95 <- gpd_quantile(0.95, u, df_evt$sigma_hat, df_evt$xi_hat)
df_evt$q99 <- gpd_quantile(0.99, u, df_evt$sigma_hat, df_evt$xi_hat)

df_evt$E_excess <- df_evt$sigma_hat / (1 - df_evt$xi_hat)
df_evt$E_y      <- u + df_evt$E_excess

df_evt$evt_score   <- df_evt$p_y_gt_1_3 * df_evt$q99 * df_evt$E_y
df_evt$evt_score_z <- as.numeric(scale(df_evt$evt_score))
