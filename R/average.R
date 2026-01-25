#' Average function with minimal valid values or maximum NAs
#'
#' Computes arithmetic means (or other averages) with minimal requirements
#' for valid values or maximum allowed NAs.
#' 
#' If the number of valid values is below `min_valid` or the number of NAs is
#' above `max_na`, the function returns NA. Otherwise, it computes the average using
#' the specified function `func`.
#'
#' @param x Vector of numeric values.
#' @param min_valid Minimal number of valid values that is required for
#'   calculating the mean. A value between 0 and 1 indicates a proportion of
#'   values (e.g., 0.5 = 50 percent of values have to be valid).
#' @param max_na Maximum number of NAs that are allowed before returning NA. A
#'   value between 0 and 1 indicates a proportion of values (e.g., 0.5 = 50
#'   percent NAs are allowed).
#' @param func A function for calculating the average. Default is `mean()`.
#' @details
#'   If both \code{min_valid} and \code{max_na} are
#'   provided, both conditions have to be met for the calculation to proceed.
#'   Missing values (NAs) are ignored in the calculation of the average.
#'   If the conditions for \code{min_valid} or \code{max_na} are not met,
#'   NA is returned.
#'   
#' @return A numeric value representing the average, or NA if the conditions are not met.
#' @author Juergen Wilbert
#' @examples
#' dat <- c(1:5, NA, NA)
#' average(dat, min_valid = 5)
#' average(dat, min_valid = 6)
#' average(dat, max_na = 2)
#' average(dat, max_na = 1)
#' average(dat, min_valid = 0.5, func = median)
#' average(dat, min_valid = 0.9)
#' @export
average <- function(x, min_valid, max_na, func = mean) {
  
  if (!missing(min_valid)) {
    if (min_valid < 1 && min_valid > 0) min_valid <- trunc(min_valid * length(x))
    if(sum(!is.na(x)) < min_valid) {
      return(NA)
    }
  }
  
  if (!missing(max_na)) {
    if (max_na < 1 && max_na > 0) max_na <- trunc(max_na * length(x))
    if(sum(is.na(x)) > max_na) {
      return(NA)
    }
  }
  func(x[which(!is.na(x))])
}

