#' Percent of TRUE values
#' 
#' Returns percentage of the occurance of TRUE values in a vector of logical values.
#'
#' @param x A vector
#' @param round Demical positions (default = 2).
#' @param min_valid Minimal number of valid values that is requiered for calculation.
#' A value between 0 and 1 indicates a proportion of values (e.g., 0.5 = 50 percent of values have to be valid).
#' @param max_na Maximum number of NAs that are allowed before returning NA.
#' A value between 0 and 1 dicates a proportion of values (e.g., 0.5 = 50 percent NAs are allowed).
#' @return A percentage
#' @export
#' @examples 
#' percent(runif(10000) > 0.50)
percent <- function(x, round = 2, min_valid, max_na) {
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
  
  mean(x, na.rm = TRUE) * 100 %>% round(digits = round)
  
}
