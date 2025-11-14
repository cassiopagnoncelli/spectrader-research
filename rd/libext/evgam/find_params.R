############################################################
# EXTREME VALUE REGRESSION – UNIFORM PIT OPTIMISATION
############################################################

suppressPackageStartupMessages({
  library(data.table)
  library(VGAM)
  library(dplyr)
  library(ggplot2)
})

############################################################
# 1. BUILD TRAINING SET
############################################################

df_train <- copy(X[train_indices])
setDT(df_train)

df_train[, y          := ys$extreme_high_identity[train_indices]]
df_train[, y_kurtosis := ys$kurtosis[train_indices]]
df_train[, y_skewness := ys$skewness[train_indices]]

# threshold
u <- quantile(df_train$y, 0.95, na.rm = TRUE)

df_evt <- df_train[y > u]
df_evt[, excess := y - u]

df_evt <- df_evt |> drop_na()

all_preds <- setdiff(names(df_evt), c("y","y_kurtosis","y_skewness","excess"))


############################################################
# 2. PRE-SCREEN PREDICTORS (TOP 40 ABS COR WITH EXCESS)
############################################################

cors <- sapply(all_preds, function(v)
  suppressWarnings(abs(cor(df_evt[[v]], df_evt$excess, use="pair")))
)

top40 <- names(sort(cors, decreasing = TRUE))[1:40]


############################################################
# 3. UNIFORMITY LOSS (AD-CVM HYBRID)
############################################################

compute_ad_loss <- function(vars) {
  form <- as.formula(paste("excess ~", paste(vars, collapse = " + ")))

  fit <- try(vglm(form, gpd(), data = df_evt), silent = TRUE)
  if (inherits(fit, "try-error")) return(Inf)

  pred <- predict(fit, type="response")
  sigma_hat <- pred[,1]
  xi_hat    <- pred[,2]

  pit <- (1 + xi_hat * df_evt$excess / sigma_hat) ^ (-1/xi_hat)
  pit <- pmin(pmax(pit, 1e-12), 1 - 1e-12)

  pit_sorted <- sort(pit)
  n <- length(pit_sorted)
  u <- (1:n - 0.5) / n

  # AD dominates tails, CVM smooths center
  ad  <- sum(((pit_sorted - u)^2) / (u * (1 - u)))
  cvm <- mean((pit_sorted - u)^2)

  return(ad + 10*cvm)
}


############################################################
# 4. FORWARD SELECTION
############################################################

selected <- c()
remaining <- top40
best_loss <- Inf

repeat {
  losses <- sapply(remaining, function(v) {
    vars <- c(selected, v)
    compute_ad_loss(vars)
  })

  best_var  <- remaining[which.min(losses)]
  best_var_loss <- min(losses)

  if (best_var_loss < best_loss) {
    selected <- c(selected, best_var)
    remaining <- setdiff(remaining, best_var)
    best_loss <- best_var_loss
  } else break
}

cat("Forward-selected predictors:\n")
print(selected)


############################################################
# 5. BACKWARD PRUNING
############################################################

repeat {
  if (length(selected) <= 1) break

  losses <- sapply(selected, function(v) {
    trial <- setdiff(selected, v)
    compute_ad_loss(trial)
  })

  best_drop_loss <- min(losses)
  drop_var <- selected[which.min(losses)]

  if (best_drop_loss < best_loss) {
    selected <- setdiff(selected, drop_var)
    best_loss <- best_drop_loss
  } else break
}

cat("Final selected predictors:\n")
print(selected)


############################################################
# 6. FIT FINAL EVT MODEL
############################################################

final_formula <- as.formula(
  paste("excess ~", paste(selected, collapse = " + "))
)

fit_evt <- vglm(final_formula, gpd(), data = df_evt)

print(summary(fit_evt))


############################################################
# 7. DIAGNOSTICS (PIT Histogram + QQ Plot)
############################################################

pred <- predict(fit_evt, type="response")
sigma_hat <- pred[,1]
xi_hat    <- pred[,2]

df_evt[, sigma_hat := sigma_hat]
df_evt[, xi_hat    := xi_hat]

df_evt[, pit := (1 + xi_hat*excess/sigma_hat)^(-1/xi_hat)]
df_evt[, pit := pmin(pmax(pit, 1e-12), 1 - 1e-12)]

# PIT histogram
ggplot(df_evt, aes(pit)) +
  geom_histogram(bins = 40, fill="steelblue", alpha=0.8) +
  theme_minimal() +
  labs(title = "Final EVT PIT Histogram")

# QQ plot
df_evt[, theoretical := qgpd((rank(excess)-0.5)/.N, scale=sigma_hat, shape=xi_hat)]
ggplot(df_evt, aes(theoretical, excess)) +
  geom_point(alpha=0.4) +
  geom_abline(color="red") +
  theme_minimal() +
  labs(title="EVT QQ Plot – Excess vs. Theoretical")


############################################################
# 8. RETURN-LEVEL ESTIMATES
############################################################

qfun <- function(alpha, u, sigma, xi) u + sigma/xi * ((1-alpha)^(-xi) - 1)

df_evt[, q95 := qfun(0.95, u, sigma_hat, xi_hat)]
df_evt[, q99 := qfun(0.99, u, sigma_hat, xi_hat)]

df_evt[, E_excess := sigma_hat / (1 - xi_hat)]
df_evt[, E_y := u + E_excess]

cat("\nDONE.\n")
