#' Add subgroup aggregates to a data frame
#'
#' Computes summary statistics for selected variables within subgroups defined
#' by the *combination* of one or more grouping variables (e.g., `age` within
#' `sex`) and merges the aggregated values back into the original data.
#' 
#' This implementation avoids repeated `merge()` calls (which can lead to
#' duplicated columns and ordering issues) by computing a stable subgroup key and
#' indexing results back to the original rows.
#' 
#' @param dat A data.frame containing the columns listed in `grouping` and
#'   `vars`.
#' @param grouping A character vector of one or more column names in `dat` that
#'   define the subgroups (their joint combinations). For example, `c("sex",
#'   "age")` yields aggregates for each `sex`-by-`age` subgroup.
#' @param vars A character vector of column names in `dat` to be aggregated.
#' @param func A list with named functions applied to each variable in `vars`
#'   within each subgroup. Default computes the mean with missing values
#'   removed.
#'
#' @details If multiple functions are provided in `func`, the
#' resulting columns are suffixed with the names of the functions in `func`. If
#' `func` is an unnamed list, suffixes "stat1", "stat2", etc. are used.
#'
#' @return A data.frame with the same observations as `dat`, plus additional
#'   columns containing subgroup-level aggregated values for each variable in
#'   `vars`.
#' @seealso [stats::aggregate()], [base::merge()]
#' @examples
#' dat <- data.frame(
#'   sex = c("f", "f", "m", "m", "m"),
#'   age = c(10, 10, 10, 12, 12),
#'   score = c(1, NA, 3, 5, 7),
#'   other = 1:5
#' )
#'
#' # Mean score per subgroup (sex x age), added back to each row
#' add_group_aggregate(dat, grouping = c("sex", "age"), vars = "score")
#'
#' # Maximum and median per subgroup
#' add_group_aggregate(
#'   dat,
#'   grouping = c("sex", "age"),
#'   vars = c("score", "other"),
#'   func = list(
#'     max = function(x) max(x, na.rm = TRUE),
#'     median = function(x) median(x, na.rm = TRUE)
#'    )
#' )
#'
#' @export
add_group_aggregate <- function(dat,
                                grouping,
                                vars,
                                func = list(mean = function(x) mean(x, na.rm = TRUE))) {


  if (is.function(func)) {
    func <- list(func)
  }

  f_names <- names(func)
  if (is.null(f_names)) f_names <- rep("", length(func))
  for (i in seq_along(f_names)) {
    if (is.na(f_names[i]) || !nzchar(f_names[i])) {
      f_names[i] <- paste0("stat", i)
    }
  }
  
  key <- interaction(dat[grouping], drop = TRUE, lex.order = TRUE, sep = "\r")
  
  # If all keys are NA (e.g., all grouping cols NA), just add NA columns
  if (all(is.na(key))) {
    for (v in vars) {
      for (i in seq_along(func)) {
        dat[[paste0(v, "_", f_names[i])]] <- NA
      }
    }
    return(dat)
  }
  
  for (v in vars) {
    x <- dat[[v]]
    
    for (i in seq_along(func)) {
      f <- func[[i]]

      stats_vec <- tapply(x, key, f)
      dat[[paste0(v, "_", f_names[i])]] <- ifelse(
        is.na(key),
        NA,
        unname(stats_vec[as.character(key)])
      )
    }
  }
  
  dat
}  
