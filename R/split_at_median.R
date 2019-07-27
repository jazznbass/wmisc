#' Dichotomize vector valeues at their median
#'
#' @param x A vector
#' @param lab Vector with two factor labels.
#' @param type "higher" will split group at above median (vs. equal and below)
#' and "lower" will split group below median (vs. equal and above)
#' @param explicit_na If not NA, NAs will be recoded as a fector level of the provided name.
#' If TRUE, the name will default to '(Missing)'.
#' @return A vector of type factor with two levels.
#' @export
#'
#' @examples
#' sample(c(1:10, NA), 100, replace = TRUE) %>%
#'   split_at_median(explicit_na = TRUE) %>%
#'   table()
#' 
split_at_median <- function(x, lab = c("low", "high"), type = "higher", explicit_na = NA) {
  if (!type %in% c("higher", "lower")) {
    stop("Wrong type. Must be 'higher' or 'lower'")
  }
  if (isTRUE(explicit_na)) explicit_na <- "(Missing)"
  md <- median(x, na.rm = TRUE)
  if (type == "higher") x <- if_else(x <= md, 1, 2)
  if (type == "lower") x <- if_else(x < md, 1, 2)
  x <- factor(x, levels = c(1, 2), labels = lab)
  if (!is.na(explicit_na)) x <- fct_explicit_na(x, na_level = explicit_na)
  x
}
