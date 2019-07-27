### confidence interval

conf_int <- function(x, ci = 0.95, r, s, se = s * sqrt(1 - r), n = NA) {
  low <- paste0((0.5 - ci / 2) * 100, "%")
  high <- paste0((0.5 + ci / 2) * 100, "%")
  Z <- qnorm(ci + ((1 - ci) / 2))
  ci.z <- c(x - Z * se, x + Z * se)
  names(ci.z) <- c(low, high)
  ci.t <- NA
  
  if (!is.na(n)) {
    t <- qt(ci + ((1 - ci) / 2), df = n - 1)
    ci.t <- c(x - t * se, x + t * se)
    names(ci.t) <- c(low, high)
  }
  out <- list()
  out$"Z" <- ci.z
  out$"t" <- ci.t
  out
}

