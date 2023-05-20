#' Calculate Confidence Interval
#'
#' Calculate the confidence interval of a given sample mean.
#'
#' @param x A sample mean
#' @param ci Numeric, confidence level (default = 0.95)
#' @param r Numeric, correlation coefficient (default = NA)
#' @param s Numeric, standard deviation of population (default = NA)
#' @param se Numeric, standard error of the sample mean (default = s * sqrt(1 - r))
#' @param n Numeric, sample size (default = NA)
#' 
#' @return A list containing confidence intervals from Z and t methods.
#'
#' @examples
#' conf_int(x = 12, ci = 0.99, s = 2.5, n = 24)
#' conf_int(x = 12, s = 2.5, r = 0.6)
#' @export
conf_int <- function(x, ci = 0.95, r = NA, s = NA, se = NA, n = NA) {

  if (is.na(se)) {
    if (is.na(s)) stop("Need s to calculate standard error")
    if (is.na(r) && is.na(n)) 
      stop("Either need n or r to calculate standard error")
    if (!is.na(r)) se <-  s * sqrt(1 - r)
    if (!is.na(n)) se <-  s / sqrt(n)
  }
  
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
  out$"Z based confidence interval" <- ci.z
  out$"t based confidence interval" <- ci.t
  cat("Confidence intervalls\n\n")
  out
}

