# Shannon entropy (differential) calculation
entropy_r <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 5 || sd(x) == 0) {
    return(NA_real_)
  }
  kd <- density(x) # KDE: returns list(x=..., y=...)
  f <- approx(kd$x, kd$y, xout = x, rule = 2)$y # interpolate f(x) at sample points
  -mean(log(pmax(f, .Machine$double.eps)))
}

# Rolling Shannon entropy (differential) like TTR::runSD
runH <- function(x, n = 100) {
  stopifnot(is.numeric(x))
  slider::slide_dbl(x, .f = entropy_r, .before = n - 1, .complete = TRUE)
}
