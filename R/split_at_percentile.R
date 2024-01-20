#' Factorizes a vector by percentiles
#'
#' @param x A vector
#' @param frac Fractions to split at (e.g., `c(1/3, 2/3)` will result in three
#'   groups and 0.5 will result in a median split)
#' @param labels Vector with factor labels.
#' @param type "higher" will split group below fraction and last group equal
#'   above last fraction. and "lower" will split group below fraction (vs. equal
#'   and above)
#' @param explicit_na If not NA, NAs will be recoded as a factor level of the
#'   provided name. If TRUE, the name will default to '(Missing)'.
#' @return A vector of type factor with two levels.
#' @examples
#' x <- sample(c(1:10, NA), 100, replace = TRUE)
#' x <- split_at_percentile(x, explicit_na = TRUE)
#' table(x)
#' 
#' split_at_percentile(
#'   1:100, 
#'   frac = c(0.25, 0.5, 0.75), 
#'   labels = c("0-24", "25-49", "50-74", "75-100")
#' )
#' 
#' @export
split_at_percentile <- function(x, 
                                frac = c(1/3, 2/3), 
                                labels = c("low", "middle", "high"), 
                                type = "higher", 
                                explicit_na = NA) {
  if (!type %in% c("higher", "lower")) {
    stop("Wrong type. Must be 'higher' or 'lower'")
  }
  
  if (isTRUE(explicit_na)) explicit_na <- "(Missing)"
  
  percentile <- quantile(x, prob = frac, na.rm = TRUE)
  new_x <- rep(NA, length(x))

  new_x[which(x < percentile[1])] <- 1
  new_x[which(x >= percentile[length(percentile)])] <- length(percentile) + 1
  for (i in 2:(length(percentile))) {
    new_x[which(x >= percentile[i - 1] & x < percentile[i])] <- i
  }
  
  new_x <- factor(new_x, levels = 1:(length(frac) + 1), labels = labels) 
  if (!is.na(explicit_na)) new_x <- forcats::fct_na_value_to_level(new_x, level = explicit_na)
  new_x
}
