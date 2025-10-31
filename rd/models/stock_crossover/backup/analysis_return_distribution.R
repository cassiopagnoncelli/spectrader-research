### -------------------------------------------------
### Gamma vs Lognormal Fit Diagnostic (Final Clean Version)
### -------------------------------------------------

pkgs <- c("MASS", "fitdistrplus", "goftest", "ggplot2", "patchwork", "gridExtra")
need <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(need) > 0) install.packages(need)

library(MASS)
library(fitdistrplus)
library(goftest)
library(ggplot2)
library(patchwork)
library(gridExtra)

compare_gamma_lognorm <- function(x, plot = TRUE) {
  # --- Validate & clean data -------------------------------------------------
  if (is.null(x) || length(x) == 0) stop("df$y is empty or missing.")
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (all(x <= 0)) x <- x - min(x) + 1e-6
  if (length(unique(x)) <= 1) stop("Need >1 distinct positive numeric value after cleaning.")

  # --- Fit distributions ----------------------------------------------------
  fit_gamma <- fitdist(x, "gamma")
  fit_logn  <- fitdist(x, "lnorm")

  # --- Information criteria -------------------------------------------------
  ic <- data.frame(
    model = c("gamma", "lognormal"),
    AIC   = c(AIC(fit_gamma), AIC(fit_logn)),
    BIC   = c(BIC(fit_gamma), BIC(fit_logn))
  )

  # --- Goodness-of-fit tests ------------------------------------------------
  ks_gamma <- ks.test(x, "pgamma",
                      shape = fit_gamma$estimate["shape"],
                      rate  = fit_gamma$estimate["rate"])
  ks_logn  <- ks.test(x, "plnorm",
                      meanlog = fit_logn$estimate["meanlog"],
                      sdlog   = fit_logn$estimate["sdlog"])
  ad_gamma <- ad.test(x, null = "pgamma",
                      shape = fit_gamma$estimate["shape"],
                      rate  = fit_gamma$estimate["rate"])
  ad_logn  <- ad.test(x, null = "plnorm",
                      meanlog = fit_logn$estimate["meanlog"],
                      sdlog   = fit_logn$estimate["sdlog"])

  winner <- if (ic$AIC[1] < ic$AIC[2]) "gamma" else "lognormal"

  # --- Plots ----------------------------------------------------------------
  if (plot) {
    dfp <- data.frame(x = x)
    base <- ggplot(dfp, aes(x)) +
      geom_histogram(aes(y = ..density..), bins = 40,
                     fill = "grey80", color = "white") +
      theme_minimal() + labs(x = "x", y = "Density")

    p_gamma <- base +
      stat_function(fun = dgamma,
                    args = as.list(fit_gamma$estimate),
                    color = "blue", linewidth = 1) +
      ggtitle("Gamma fit")

    p_logn <- base +
      stat_function(fun = dlnorm,
                    args = as.list(fit_logn$estimate),
                    color = "darkorange3", linewidth = 1) +
      ggtitle("Lognormal fit")

    # Convert fitdistrplus objects into ggplot objects manually
    qq_gamma <- ggplot(data.frame(
      theo = qgamma(ppoints(length(x)),
                    shape = fit_gamma$estimate["shape"],
                    rate  = fit_gamma$estimate["rate"]),
      sample = sort(x)
    ), aes(x = theo, y = sample)) +
      geom_point(size = 1.3, alpha = 0.6, color = "blue") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      theme_minimal() + ggtitle("QQ - Gamma")

    qq_logn <- ggplot(data.frame(
      theo = qlnorm(ppoints(length(x)),
                    meanlog = fit_logn$estimate["meanlog"],
                    sdlog   = fit_logn$estimate["sdlog"]),
      sample = sort(x)
    ), aes(x = theo, y = sample)) +
      geom_point(size = 1.3, alpha = 0.6, color = "darkorange3") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      theme_minimal() + ggtitle("QQ - Lognormal")

    grid.arrange(p_gamma, p_logn, qq_gamma, qq_logn, ncol = 2)
  }

  list(
    params = list(gamma = fit_gamma$estimate, lognormal = fit_logn$estimate),
    info_criteria = ic,
    ks = data.frame(model = c("gamma", "lognormal"),
                    stat = c(ks_gamma$statistic, ks_logn$statistic),
                    p = c(ks_gamma$p.value, ks_logn$p.value)),
    ad = data.frame(model = c("gamma", "lognormal"),
                    stat = c(ad_gamma$statistic, ad_logn$statistic),
                    p = c(ad_gamma$p.value, ad_logn$p.value)),
    winner_by_AIC = winner
  )
}

### -------------------------------------------------
### USAGE
### -------------------------------------------------
if (!("y" %in% names(df)) || length(df$y) == 0) {
  message("df$y missing or empty — generating synthetic data for demo.")
  df$y <- rgamma(20000, shape = 3, rate = 1)
}

out <- compare_gamma_lognorm(df$y, plot = TRUE)
out$info_criteria
out$winner_by_AIC
