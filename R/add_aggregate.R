#' Add subgroup aggregates to a data frame
#'
#' Computes summary statistics for selected variables within subgroups defined by
#' the *combination* of one or more grouping variables (e.g., `age` within `sex`)
#' and merges the aggregated values back into the original data.
#'
#' @param dat A data.frame containing the columns listed in `grouping` and `vars`.
#' @param grouping A character vector of one or more column names in `dat` that
#'   define the subgroups (their joint combinations). For example,
#'   `c("sex", "age")` yields aggregates for each `sex`-by-`age` subgroup.
#' @param vars A character vector of column names in `dat` to be aggregated.
#' @param func A list with named functions applied to each variable in `vars` within each subgroup.
#'   Must accept a vector and return a length-1 value. Default computes the mean with
#'   missing values removed.
#'
#' @details
#' Aggregation is performed using [stats::aggregate()] with `by = dat[, grouping]`,
#' so each unique combination of the `grouping` variables defines a subgroup.
#' Results are joined back to `dat` using [base::merge()] by all `grouping` columns.
#'
#' @return A data.frame with the same observations as `dat`, plus additional columns containing subgroup-level
#'   aggregated values for each variable in `vars`.
#'
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
                                func = list(
                                  mean = function(x) mean(x, na.rm = TRUE))
                                ) {
  
  rid <- "..tmp_variable.."
  dat[[rid]] <- seq_len(nrow(dat))

  if (!is.list(func)) func <- list(func)
  dat_cols <- dplyr::select(dat, {{ vars }})
  for(i in 1:length(func)) {
    dat_aggregates <- aggregate(
      dat_cols,
      by = dat[, grouping, drop = FALSE],
      FUN = func[[i]]
    )
    suf <- names(func)[i]
    if (is.null(suf)) suf <- paste0("stat", i)
    dat <- merge(
      dat, 
      dat_aggregates, 
      by = grouping, 
      all.x = TRUE, 
      sort = FALSE, 
      suffixes = c("", paste0("_", suf))
    )
   
  }
  
  dat <- dat[order(dat[[rid]]), , drop = FALSE]
  dat[[rid]] <- NULL
  
  dat
}
