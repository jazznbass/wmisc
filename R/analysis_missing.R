#' Analyse Missing Values in Scales
#'
#' This function analyzes missing values for specified scales within a data frame.
#'
#' @param dat A data frame containing the dataset to be analyzed.
#' @param scale A list of character vectors, where each list element contains the
#'   variable names corresponding to a scale within the dataset.
#'
#' @return A data frame summarizing the missing value analysis for each scale. The output includes:
#' - `missing`: The total number of missing values for each scale.
#' - `total`: The total number of observations expected for each scale.
#' - `p`: The proportion of missing values for each scale (`missing / total`).
#' - `n cases`: The number of cases with at least one missing value within the scale.
#' - `p cases`: The proportion of cases with at least one missing value (`n cases / total cases`).
#' - `n all cases`: The number of cases where all variables in the scale are missing.
#' - `p all cases`: The proportion of cases where all variables in the scale are missing (`n all cases / total cases`).
#'
#' @examples
#' # Example dataset
#' dat <- data.frame(
#'   scale1_var1 = c(1, 2, NA, 4),
#'   scale1_var2 = c(NA, 2, 3, 4),
#'   scale2_var1 = c(1, NA, 3, 4),
#'   scale2_var2 = c(NA, NA, NA, 4)
#' )
#'
#' # Define scales
#' scales <- list(
#'   scale1 = c("scale1_var1", "scale1_var2"),
#'   scale2 = c("scale2_var1", "scale2_var2")
#' )
#'
#' # Analyze missing values
#' analysis_missing(dat, scales)
#'
#' @seealso [is.na()], [apply()]
#' @export
analysis_missing <- function(dat, scale) {
  N <- length(scale)
  cases <- nrow(dat)
  p.total <- data.frame("missing" = NA, "total" = NA, "p" = NA)
  for (i in 1:N) {
    n <- length(scale[[i]]) * nrow(dat)
    p.total[i, "missing"] <- sum(is.na(dat[, scale[[i]]]))
    p.total[i, "total"] <- length(scale[[i]]) * nrow(dat)
    p.total[i, "p"] <- sum(is.na(dat[, scale[[i]]])) / (length(scale[[i]]) * nrow(dat))
    p.total[i, "n cases"] <- sum(unlist(apply(dat[, scale[[i]]], 1, function(x) any(is.na(x)))))
    p.total[i, "p cases"] <- sum(unlist(apply(dat[, scale[[i]]], 1, function(x) any(is.na(x))))) / cases
    p.total[i, "n all cases"] <- sum(unlist(apply(dat[, scale[[i]]], 1, function(x) all(is.na(x)))))
    p.total[i, "p all cases"] <- sum(unlist(apply(dat[, scale[[i]]], 1, function(x) all(is.na(x))))) / cases
  }
  p.total
}
