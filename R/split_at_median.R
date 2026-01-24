#' Dichotomize vector values at their median value
#' 
#' Splits a numeric vector into two groups at the median value. Values above
#' (or below) the median are assigned to one group, values equal to or below
#' (or above) the median are assigned to the other group. Optionally, missing
#' values can be assigned to a separate factor level.
#' 
#' @details
#'   This function is a wrapper around \code{split_at_percentile()} with the
#'   percentile fixed at 0.5 (the median). See
#'   \code{\link{split_at_percentile()}} for more details.
#'
#' @param x A vector
#' @param labels Vector with two factor labels.
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
    x, 0.5, labels = labels, type = type, explicit_na = explicit_na
  )
}
