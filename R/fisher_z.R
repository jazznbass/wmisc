
#' @export
fisher_z <- function(r) 0.5 * log((1 + r) / (1 - r))

#' @export
#' @rdname fisher_z
inv_fisher_z <- function(z) {
  (exp(2 * z) -1) / (exp(2 * z) + 1)
}
