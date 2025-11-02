kelly_fraction <- function(rets) {
  rets <- rets[is.finite(rets)]
  p <- mean(rets > 0)
  q <- 1 - p
  win <- rets[rets > 0]
  loss <- rets[rets < 0]
  if (length(win) == 0) return(0)
  if (length(loss) == 0) return(1)
  b <- mean(win) / abs(mean(loss))
  f <- (b * p - q) / b
  max(0, min(1, f))
}

plot_kelly_trades <- function(returns, kelly_p = NULL, log.transform = FALSE) {
  if (is.null(kelly_p)) {
    kelly_p <- kelly_fraction(returns)
  }

  portfolio <- cumprod(c(1, 1 + returns * kelly_p))
  if (log.transform) {
    portfolio <- log(portfolio)
  }
  df <- data.frame(
    trade = seq_along(portfolio),
    portfolio = portfolio
  ) %>% na.omit()

  p <- ggplot2::ggplot(df, ggplot2::aes(x = trade, y = portfolio)) +
    ggplot2::geom_area(fill = "chartreuse4", alpha = 0.25) +
    ggplot2::geom_line(linewidth = 0.25, color = "chartreuse4") +
    ggplot2::geom_hline(yintercept = 1, linetype = "dotted", color = "gray50") +
    ggplot2::labs(
      title = "Kelly Portfolio Growth",
      subtitle = sprintf("%s, %d trades, f* = %.3f",
        ifelse(log.transform, "log(W_n)", "W_n"),
        nrow(df),
        kelly_p
      ),
      x = "Trade Number",
      y = "Portfolio Value"
    ) +
    ggplot2::theme_minimal()
  print(p)
  p
}
