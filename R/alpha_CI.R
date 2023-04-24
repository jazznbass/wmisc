#' alpha_CI
#' 
#' Calculate the confidence interval for Cronbach's alpha
#'
#' @param alpha Alpha value
#' @param n Sample size
#' @param items Number of items
#' @param ci Confidence level (e.g. 0.95 for 95 percent)
#' 
#' @return a numeric vector containing the upper and lower bounds of the confidence interval
#' 
#' @examples
#' alpha_CI(0.8, 10, 50, 0.99)
#' 
#' @importFrom stats qf
#' 
#' @export
alpha_CI <- function(alpha, n, items, ci) {
  out <- 1 - (1 - alpha) * qf(c(1 - (1 - ci) / 2, (1 - ci) / 2), n - 1, (n - 1) * (items - 1))
  out
}

