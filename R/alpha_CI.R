#' Cronbachs Alpha Confidence Interval
#'
#' @param alpha Alpha value
#' @param n Sample size
#' @param items Number of items
#' @param ci Confidence level (e.g. 0.95 for 95%)
#' @export
alpha_CI <- function(alpha, n, items, ci) {
  out <- 1 - (1 - alpha) * qf(c(1 - (1 - ci) / 2, (1 - ci) / 2), n - 1, (n - 1) * (items - 1))
  out
}

