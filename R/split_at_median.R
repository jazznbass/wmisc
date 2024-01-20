#' Dichotomize vector values at their median
#'
#' @param x A vector
#' @param lab Vector with two factor labels.
#' @param type "higher" will split group at above median (vs. equal and below)
#'   and "lower" will split group below median (vs. equal and above)
#' @param explicit_na If not NA, NAs will be recoded as a factor level of the
#'   provided name. If TRUE, the name will default to '(Missing)'.
#' @return A vector of type factor with two levels.
#' @examples
#' x <- sample(c(1:10, NA), 100, replace = TRUE)
#' x <- split_at_median(x, explicit_na = TRUE)
#' table(x)
#'
#' @export
split_at_median <- function(x, 
                            labels = c("low", "high"), 
                            type = "higher", 
                            explicit_na = NA) {
  
  split_at_percentile(
    x, 0.5, lab = labels, type = type, explicit_na = explicit_na
  )
}
