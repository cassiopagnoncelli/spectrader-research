library(VGAM)
library(dplyr)
library(ggplot2)

df_train <- X[train_indices]
setDT(df_train)

df_train[, y := ys$excursion_high[train_indices]]
df_train[, y_kurtosis := ys$kurtosis[train_indices]]
df_train[, y_skewness := ys$skewness[train_indices]]


###### EVT MODELING WITH VGAM ######
u <- 1.30

df_ext <- df_train %>%
  as.data.frame() %>%
  filter(y > u) %>%
  mutate(excess = y - u)

cat("Exceedances:", nrow(df_ext), "\n")

#
# ============ Feature SELECTION ============ #
#

# Predictor selection
# Basic
predictors <- c(
  "vol", "vix", "H", "H_slow",
  "ae_recon_error", "ae_volatility", "R_1"
)
evt_formula <- excess ~ vol + vix + H + H_slow + ae_recon_error + ae_volatility + R_1

# Incorporated
predictors <- setdiff(
  colnames(df_train),
  c(
    "y", "excess",
    "smoothed_close", "smoothed_close_1", "smoothed_close_2",
    "rsi_2"
  )
)[50:55]
evt_formula <- as.formula(
  paste("excess ~", paste(predictors, collapse = " + "))
)
evt_formula

df_ext_mod <- df_ext %>%
  select(excess, all_of(predictors)) %>%
  drop_na()

#
# ============ MODEL FITTING ============ #
#

# Fit model
fit_vgam <- vglm(
  evt_formula,
  gpd(),
  data = df_ext_mod
)

summary(fit_vgam)
