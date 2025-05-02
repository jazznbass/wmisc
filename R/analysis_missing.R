#' Analyze Missing Values in Scales
#'
#' Summarizes missing values across user-defined scales in a data frame.
#'
#' @param dat A data frame containing the data to be analyzed.
#' @param scale A named list of character vectors. Each list element defines a scale
#'   by specifying variable names (column names in `dat`) belonging to that scale.
#'
#' @return A data frame summarizing the missing value statistics for each scale with the following columns:
#' \describe{
#'   \item{missing}{Total number of missing values across variables in the scale.}
#'   \item{total}{Total number of values expected (cases × number of variables in scale).}
#'   \item{p}{Proportion of missing values (`missing / total`).}
#'   \item{n cases}{Number of cases with at least one missing value in the scale.}
#'   \item{p cases}{Proportion of such cases (`n cases / total cases`).}
#'   \item{n all cases}{Number of cases where all scale variables are missing.}
#'   \item{p all cases}{Proportion of cases where all scale variables are missing (`n all cases / total cases`).}
#' }
#'
#' @examples
#' dat <- data.frame(
#'   scale1_var1 = c(1, 2, NA, 4),
#'   scale1_var2 = c(NA, 2, 3, 4),
#'   scale2_var1 = c(1, NA, 3, 4),
#'   scale2_var2 = c(NA, NA, NA, 4)
#' )
#'
#' scales <- list(
#'   scale1 = c("scale1_var1", "scale1_var2"),
#'   scale2 = c("scale2_var1", "scale2_var2")
#' )
#'
#' analysis_missing(dat, scales)
#'
#' @seealso [is.na()], [complete.cases()]
#' @export
analysis_missing <- function(dat, scale) {
  cases <- nrow(dat)
  
  res <- lapply(scale, function(vars) {
    dat_sub <- dat[, vars, drop = FALSE]
    n_total <- length(vars) * cases
    n_missing <- sum(is.na(dat_sub))
    n_cases_any <- sum(apply(dat_sub, 1, function(x) any(is.na(x))))
    n_cases_all <- sum(apply(dat_sub, 1, function(x) all(is.na(x))))
    
    data.frame(
      missing = n_missing,
      total = n_total,
      p = n_missing / n_total,
      `n cases` = n_cases_any,
      `p cases` = n_cases_any / cases,
      `n all cases` = n_cases_all,
      `p all cases` = n_cases_all / cases,
      check.names = FALSE
    )
  })
  
  out <- do.call(rbind, res)
  rownames(out) <- names(scale)
  out
}
