#' Standardise values
#'
#' @aliases standardise standardize
#' @param x A vector
#' @param grouping A variable with group values for calculating grouped centered.
#' @param min_valid Minimal number of valid values that is requiered for calculation.
#' A value between 0 and 1 indicates a proportion of values (e.g., 0.5 = 50 percent of values have to be valid).
#' @param max_na Maximum number of NAs that are allowed before returning NA.
#' A value between 0 and 1 dicates a proportion of values (e.g., 0.5 = 50 percent NAs are allowed).
#' @param m Mean after standardisation (default = 0)
#' @param sd Mean after standardisation (default = 1)
#' @export
#' @examples 
#' standardise( c(1:9, NA, NA, NA, NA, NA), m = 100, sd = 15)
#' standardise( c(1:95, NA, NA, NA, NA, NA), min_valid = 0.96)
standardise <- function(x, grouping, min_valid, max_na, m = 0, sd = 1) {

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
    s_g <- tapply(x, grouping, sd, na.rm = TRUE)
    s_g <- sapply(grouping, function(x) s_g[names(s_g) == x]) %>% unname()
  } else {
    m_g <- mean(x, na.rm = TRUE)
    s_g <- sd(x, na.rm = TRUE)
  }
  
  (x - m_g) / s_g * sd + m

}

#' @export
standardize <- function(...) standardise(...)

