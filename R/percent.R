#' Percent of TRUE values
#'
#' Returns percentage of the occurance of TRUE values in a vector of logical
#' values.
#' 
#' If `min_valid` is provided, the function will only calculate the percentage
#' if the number of valid (non-NA) values is at least `min_valid`. 
#' If `max_na` is provided, the function will only calculate the
#' percentage if the number of NAs is at most `max_na`. If the conditions
#' are not met, NA is returned.
#'
#' @param x A vector
#' @param min_valid Minimal number of valid values that is required for
#'   calculation. A value between 0 and 1 indicates a proportion of values
#'   (e.g., 0.5 = 50 percent of values have to be valid).
#' @param max_na Maximum number of NAs that are allowed before returning NA. A
#'   value between 0 and 1 indicates a proportion of values (e.g., 0.5 = 50
#'   percent NAs are allowed).
#' @return A percentage
#' @export
#' @examples
#' percent(runif(10000) > 0.50)
percent <- function(x, min_valid, max_na) {
  if (!is.logical(x)) {
    abort("x has to be a logical vector")
  }
  average(
    x, 
    min_valid = min_valid,
    max_na = max_na,
    func = function(x) mean(x) * 100
  )
}
