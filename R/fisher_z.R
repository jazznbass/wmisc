#' Fisher's Z Transformation
#'
#' The Fisher's Z transformation is a common method for stabilizing the variance
#' of the sample correlation coefficient.
#'
#' @param r The correlation coefficient to be transformed.
#'
#' @return The Fisher's Z transformed value.
#'
#' @details The Fisher's Z transformation is defined as: \code{0.5 * log((1 + r)
#' / (1 - r))}
#'
#' @examples
#' # Example usage:
#' r <- 0.8
#' fisher_z(r)
#'
#' @seealso \code{\link{cor}}, \code{\link{cor.test}},
#' \code{\link{inv_fisher_z}}
#'
#' @references Fisher, R. A. (1915). Frequency distribution of the values of the
#' correlation coefficient in samples from an indefinitely large population.
#' \emph{Biometrika}, 10(4), 507-521.
#'
#' @author Your Name
#'
#' @export
fisher_z <- function(r) {
  0.5 * log((1 + r) / (1 - r))
}

#' Inverse Fisher's Z Transformation
#'
#' The inverse Fisher's Z transformation is used to convert a Fisher's Z value
#' back to a correlation coefficient.
#'
#' @param z The Fisher's Z transformed value.
#'
#' @return The original correlation coefficient.
#'
#' @details The inverse Fisher's Z transformation is defined as: \code{(exp(2 *
#' z) - 1) / (exp(2 * z) + 1)}
#'
#' @examples
#' # Example usage:
#' z <- 0.972
#' inv_fisher_z(z)
#'
#' @seealso \code{\link{cor}}, \code{\link{cor.test}}, \code{\link{fisher_z}}
#'
#' @author Your Name
#'
#' @export
#' @rdname fisher_z
inv_fisher_z <- function(z) {
  (exp(2 * z) - 1) / (exp(2 * z) + 1)
}
