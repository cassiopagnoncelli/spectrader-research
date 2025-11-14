library(GA)

predictor_pool <- setdiff(colnames(df_ex), c("y","y_kurtosis","y_skewness","excess"))

K1 <- 5   # scale vars
K2 <- 2   # shape vars

loss_function <- function(bitmask) {
  mask <- bitmask == 1
  vars <- predictor_pool[mask]

  if (length(vars) < 2) return(1e6)
  if (length(vars) > K1 + K2) return(1e6)

  evt_formula <- as.formula(
    paste("excess ~", paste(vars, collapse = " + "))
  )

  fit <- try(
    vglm(evt_formula, gpd(), data=df_ex, silent=TRUE),
    silent=TRUE
  )
  if (inherits(fit, "try-error")) return(1e5)

  pred <- predict(fit, type="response")
  sigma <- pred[,1]; xi <- pred[,2]

  pit <- (1 + xi * df_ex$excess / sigma)^(-1/xi)
  pit <- pmin(pmax(pit, 1e-12), 1 - 1e-12)

  pit_loss(pit)
}

ga_model <- ga(
  type = "binary",
  fitness = function(x) -loss_function(x), # maximize fitness = minimize loss
  nBits = length(predictor_pool),
  popSize = 60,
  maxiter = 80,
  run = 40,
  pmutation = 0.2
)
