#' Merge Multiple Level Data
#'
#' Merges two data frames with different levels to create a single data frame
#'
#' @param dat_l1  Level 1 data frame containing data to merge
#' @param dat_l2  Level 2 data frame containing data to merge
#' @param id String with variable name for matching datasets
#' @param var_agg Vector with strings of L1 variables that are aggregated at
#'   Level2 with the `agg_func` function.
#' @param suffix_l A character vector of suffixes for the merged Level 2 columns
#'   (default: c('.l1', '.l2'))
#' @param suffix_m A character vector of suffixes for the merged aggregated
#'   columns (default: c('', '.mean'))
#' @param agg_func The function used in aggregation (default: function(x)
#'   mean(x, na.rm = TRUE))
#' @examples
#' merge_multilevel(mtcars, var_agg = c("mpg", "disp"), id = "cyl")
#' 
#' @return Returns a merged data frame
#'
#' @export
merge_multilevel <- function(dat_l1, 
                             dat_l2 = NULL, 
                             id, 
                             var_agg,
                             suffix_l = c(".l1", ".l2"),
                             suffix_m = c("", ".mean"),
                             agg_func = function(x) mean(x, na.rm = TRUE)) {
  
  dat_means <- aggregate(
    dat_l1[, var_agg], by = list(dat_l1[[id]]), 
    FUN = agg_func
  )
                                                        
  names(dat_means)[1] <- id
  dat_l1 <- merge(dat_l1, dat_means, by = id, suffix = suffix_m)
  if (is.null(dat_l2)) {
    out <- dat_l1
  } else {
    out <- merge(dat_l1, dat_l2, by = id, suffix = suffix_l)
  }
  out
}
