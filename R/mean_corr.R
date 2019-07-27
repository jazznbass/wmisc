#' Average correlations by Fisher Z transformation
#'
#' @param r A vector of correlation coeficients
#'
#' @return The average correlation
#' @export

mean_corr <- function(r) {
  z <- 0.5 * log((1 + r) / (1 - r))
  mz <- mean(z, na.rm = TRUE)
  (exp^(2 * mz) - 1) / (exp^(2 * mz) + 1)
}
