library(dplyr)
library(ggplot2)
library(quantreg)

# --- fit model ---------------------------------------------------------------
form <- S ~ r + sd_ratio + sd_short_1 + vix + vol_vix
form <- S ~ r + sd_short + sd_long + sd_ratio + h_short + h_long + h_ratio +
  sd_short_1 + sd_long_1 + sd_ratio_1 + h_short_1 + h_long_1 + h_ratio_1 +
  vix + vol_vix + cr_3 + cr_8

tau <- 0.92

qrfit92 <- rq(form, tau = tau, data = train_df)
sum_qr <- summary(qrfit92, se = "boot", R = 500)
sum_qr

# --- compute diagnostics -----------------------------------------------------
pred <- predict(qrfit92, train_df)
actual <- train_df$S

# Pseudo-R² (Koenker–Machado)
pred <- predict(qrfit92, train_df)
actual <- train_df$S
rho <- function(u, tau) sum(u * (tau - (u < 0)))
rho_full <- rho(actual - pred, tau)
rho_null <- rho(actual - quantile(actual, tau), tau)
R2_pseudo <- 1 - rho_full / rho_null
R2_pseudo

# Pinball loss (quantile loss)
pinball_loss <- mean((tau - (actual < pred)) * (actual - pred))

# Coverage (empirical quantile frequency)
coverage <- mean(actual <= pred)

cat(
  "Diagnostics for τ =", tau, "\n",
  "Pseudo-R²:   ", round(R2_pseudo, 4), "\n",
  "Pinball loss:", round(pinball_loss, 6), "\n",
  "Coverage:    ", round(coverage, 4), "\n"
)

# --- merge fitted values into frame ------------------------------------------
train_df <- train_df %>%
  mutate(
    qhat = pred,
    exit_flag = S >= qhat,
    residual = S - qhat
  )

# --- main path plot ----------------------------------------------------------
ggplot(train_df, aes(x = t, y = S)) +
  geom_line(aes(color = factor(position_id)), linewidth = 0.7, alpha = 0.6) +
  geom_line(aes(y = qhat), color = "black", linewidth = 0.9, linetype = "dashed") +
  geom_point(
    data = subset(train_df, exit_flag),
    color = "magenta", size = 1.8, alpha = 0.8
  ) +
  labs(
    title = paste0("Quantile Regression Upper Envelope (τ = ", tau, ")"),
    subtitle = "Dashed = fitted 92nd-quantile; magenta dots = exit triggers",
    x = "Time (t)", y = "Normalised Price (S)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# --- calibration plot --------------------------------------------------------
ggplot(data.frame(pred, actual), aes(x = pred, y = actual)) +
  geom_point(alpha = 0.25, color = "gray40") +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 0.8) +
  labs(
    title = paste0("Predicted vs Actual (τ = ", tau, ")"),
    subtitle = "Red line = perfect calibration",
    x = "Predicted Quantile", y = "Actual S"
  ) +
  theme_minimal(base_size = 12)
