#' Average
#'
#' Computes arithmetic means
#'
#' @param x Vector
#' @param min_valid Minimal number of valid values that is required for
#'   calculating the mean. A value between 0 and 1 indicates a proportion of
#'   values (e.g., 0.5 = 50 percent of values have to be valid).
#' @param max_na Maximum number of NAs that are allowed before returning NA. A
#'   value between 0 and 1 indicates a proportion of values (e.g., 0.5 = 50
#'   percent NAs are allowed).
#' @param func Function without the header `function(.)` for calculating the
#'   average.
#' @export
#' @examples
#' dat <- c(1:5, NA, NA)
#' average(dat, min_valid = 5)
#' average(dat, min_valid = 6)
#' average(dat, max_na = 2)
#' average(dat, max_na = 1)
#' average(dat, min_valid = 0.5, func = median(.))
#' average(dat, min_valid = 0.9)

average <- function(x, min_valid, max_na, func = mean(.)) {
  
  envi <- new.env()
  
  func <- deparse(substitute(func))
 
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
  envi$. <- x[which(!is.na(x))]
  eval(str2lang(func), envir = envi)
}
