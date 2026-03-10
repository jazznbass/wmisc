#' Mean center a variable
#' 
#' This function mean centers a variable, optionally within groups. It also
#' allows to set minimal valid values or maximal allowed NAs for the calculation.
#' If the conditions are not met, NA values are returned.
#' 
#' @return A vector with mean centered values
#'
#' @param x A vector of numeric values to be mean centered.
#' @param min_valid Minimal number of valid values that is required for
#'   calculation. A value between 0 and 1 indicates a proportion of values
#'   (e.g., 0.5 = 50 percent of values have to be valid).
#' @param max_na Maximum number of NAs that are allowed before returning NA. A
#'   value between 0 and 1 indicates a proportion of values (e.g., 0.5 = 50
#'   percent NAs are allowed).
#' @param grouping A variable with group values for calculating grouped
#'   centers.
#' @details
#'    If both \code{min_valid} and \code{max_na} are provided, both conditions
#'    have to be met for the calculation to proceed.
#'    If \code{grouping} is provided, the mean centering is done
#'    within the groups defined by \code{grouping}.
#'    Missing values (NAs) are ignored in the calculation of means.
#'    If the conditions for \code{min_valid} or \code{max_na} are not met,
#'    NA values are returned for all elements of \code{x}.
#' @author Juergen Wilbert
#' @export
#' @examples
#' center(c(1:10, NA, NaN))
#' center(mtcars$mpg, grouping = mtcars$cyl)
#' center(c(1:5, NA, NA), min_valid = 4)
#' center(c(1:5, NA, NA), max_na = 1)
#' center(c(1:5, NA, NA), min_valid = 0.7)
#' center(c(1:5, NA, NA), max_na = 0.3)
#' center(c(1:5, NA, NA), min_valid = 0.7, max_na = 1)
#' center(c(1:5, NA, NA), min_valid = 4, max_na = 0)
#' 
center <- function(x, min_valid, max_na, grouping) {
  if (!missing(min_valid)) {
    if (min_valid < 1 && min_valid > 0) min_valid <- trunc(min_valid * length(x))
    if(sum(!is.na(x)) < min_valid) {
      return(rep(NA, length(x)))
    }
  }
  
  if (!missing(max_na)) {
    if (max_na < 1 && max_na > 0) max_na <- trunc(max_na * length(x))
    if(sum(is.na(x)) > max_na) {
      return(rep(NA, length(x)))
    }
  }
  
  if (!missing(grouping)) {
    m_g <- tapply(x, grouping, mean, na.rm = TRUE)
    m_g <- sapply(grouping, function(x) m_g[names(m_g) == x])  |>  unname()
  } else {
    m_g <- mean(x, na.rm = TRUE)
  }
  x - m_g
}

