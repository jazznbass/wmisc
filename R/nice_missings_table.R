#' Analyze Missing Values in Scales
#'
#' Summarizes missing values across user-defined scales in a data frame.
#' 
#' @param dat A data frame containing the data to be analyzed.
#' @param scale A named list of character vectors. Each list element defines a scale
#'   by specifying variable names (column names in `dat`) belonging to that scale.
#' @param ... Further arguments passed to the `nice_table()` function for formatting the output.
#' @return A data frame summarizing the missing value statistics for each scale with the following columns:
#' \describe{
#'   \item{Missing}{Number and percentage of missing values across variables in the scale.}
#'   \item{Complete cases}{Number and percentage of cases with no missing value in the scale.}
#'   \item{All missing cases}{Number and percentage of cases where all scale variables are missing.}
#' }
#' @details
#' This function is useful for assessing the extent of missing data within specific scales
#' of a dataset. It provides insights into both the overall missings and the distribution
#' of missing values across cases for each defined scale.
#' @author
#' Jürgen Wilbert
#' @examples
#' dat <- data.frame(
#'   scale1_var1 = c(1, 2, NA, 4,NA),
#'   scale1_var2 = c(NA, 2, NA, 4,NA),
#'   scale2_var1 = c(1, NA, 3, 4, 1),
#'   scale2_var2 = c(NA, NA, NA, 4, 1)
#' )
#'
#' scales <- list(
#'   scale1 = c("scale1_var1", "scale1_var2"),
#'   scale2 = c("scale2_var1", "scale2_var2")
#' )
#'
#' nice_missings_table(dat, scales)
#'
#' @seealso [is.na()], [complete.cases()]
#' @export
nice_missings_table <- function(dat, scale, ...) {
  cases <- nrow(dat)
  
  res <- lapply(scale, function(vars) {
    dat_sub <- dat[, vars, drop = FALSE]
    n_total <- length(vars) * cases
    n_missing <- sum(is.na(dat_sub))
    n_cases_non <- sum(apply(dat_sub, 1, function(x) !any(is.na(x))))
    n_cases_all <- sum(apply(dat_sub, 1, function(x) all(is.na(x))))
    
    data.frame(
      `n` = n_missing,
      `%` = n_missing / n_total * 100,
      ` n ` = n_cases_non,
      ` % ` = n_cases_non / cases * 100,
      `  n  ` = n_cases_all,
      `  %  ` = n_cases_all / cases * 100,
      check.names = FALSE
    )
  })
  
  out <- do.call(rbind, res)
  rownames(out) <- names(scale)
  out <- set_wmisc_attributes(
    out,
    spanner = list("Missing" = 2:3, "Complete cases" = 4:5, 
                   "All missing cases" = 6:7)
  )
  nice_table(out, ...)
}
