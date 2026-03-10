#' Factorizes a vector by percentiles
#' 
#' Splits a numeric vector into groups at specified percentiles. Values below
#' (or above) the percentiles are assigned to one group, values equal to or
#' above (or below) the percentiles are assigned to the other groups. Optionally
#' missing values can be assigned to a separate factor level.
#' 
#' @details
#' This function computes the specified percentiles of the input vector and
#' assigns each value to a group based on these percentiles. The resulting
#' groups are returned as a factor with the specified labels. The
#' \code{type} parameter determines whether values equal to the percentile
#' thresholds are included in the lower or higher group.
#' 
#' Common splits can be specified using character strings for the `frac`
#' parameter:
#' 
#' - "median": splits at the 50th percentile
#' - "tertile": splits at the 33.3rd and 66.6th percentiles
#' - "quartile": splits at the 25th, 50th and 75th percentiles
#' - "quintile": splits at the 20th, 40th, 60th and 80th percentiles
#' - "decile": splits at the 10th, 20th, ..., 90th percentiles
#' 
#' The `labels` parameter should contain one more label than the number of
#' percentiles specified in `frac`, as it defines the labels for each
#' resulting group.
#'  
#'  If `explicit_na` is provided (not NA), missing values in the input
#'  vector will be recoded as a separate factor level with the specified name.
#'  If `explicit_na` is set to TRUE, the name will default to '(Missing)'.
#'
#' @param x A vector.
#' @param frac A numeric vector with percentiles (between 0 and 1) at which to split
#'  the vector. Alternatively, use character strings "median", "tertile",
#'  "quartile", "quintile", or "decile" for common splits.
#' @param labels Vector with factor labels.
#' @param type "higher" will split group below fraction and last group equal
#'   above last fraction. and "lower" will split group below fraction (vs. equal
#'   and above).
#' @param explicit_na If not NA, NAs will be recoded as a factor level of the
#'   provided name. If TRUE, the name will default to '(Missing)'.
#' @return A vector of type factor with two levels.
#' @examples
#' ## Generate sample data
#' x <- sample(c(1:100, NA), 1000, replace = TRUE) 
#' 
#' ## Ternary split
#' split_at_percentile(x, "tertile", explicit_na = TRUE) |> table()
#' 
#' ## Quartile split with custom labels
#' split_at_percentile(
#'   x,
#'   frac = c(0.25, 0.5, 0.75), 
#'   labels = c("0-24", "25-49", "50-74", "75-100")
#' ) |> table()
#' 
#' ## Quintile split
#' split_at_percentile(x, frac = "quintile") |> table() |> prop.table() |> round(2)
#'  
#' @export
split_at_percentile <- function(x, 
                                frac, 
                                labels, 
                                type = "higher", 
                                explicit_na = NA) {
  if (!type %in% c("higher", "lower")) {
    abort("Wrong type. Must be 'higher' or 'lower'")
  }
  
  if (is.character(frac)) {
    labels <- switch(frac,
                     "median" = c("lower", "upper"),
                     "tertile" = c("low", "middle", "high"),
                     "quartile" = c("quantile 1", "quantile 2", "quantile 3", "quantile 4"),
                     "quintile" = c("quintile 1", "quintile 2", "quintile 3", "quintile 4", "quintile 5"),
                     "decile" = paste0("decile ", 1:10)
    )
    frac <- switch(frac,
           "median" = 0.5,
           "tertile" = c(1/3, 2/3),
           "quartile" = c(0.25, 0.5, 0.75),
           "quintile" = c(0.2, 0.4, 0.6, 0.8),
           "decile" = seq(0.1, 0.9, by = 0.1),
           abort("Unknown character for 'frac'. Use 'median', 'tertile', 'quartile', 'quintile', or 'decile'.")
    )
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
