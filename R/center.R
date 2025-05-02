#' Mean center a variable
#'
#' @param x A vector
#' @param min_valid Minimal number of valid values that is required for
#'   calculation. A value between 0 and 1 indicates a proportion of values
#'   (e.g., 0.5 = 50 percent of values have to be valid).
#' @param max_na Maximum number of NAs that are allowed before returning NA. A
#'   value between 0 and 1 indicates a proportion of values (e.g., 0.5 = 50
#'   percent NAs are allowed).
#' @param grouping A variable with group values for calculating grouped
#'   centers.
#' @export
#' @examples
#' center(c(1:10, NA, NaN))
#' center(mtcars$mpg, grouping = mtcars$cyl)
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
    m_g <- sapply(grouping, function(x) m_g[names(m_g) == x]) %>% unname()
  } else {
    m_g <- mean(x, na.rm = TRUE)
  }
  x - m_g
}

