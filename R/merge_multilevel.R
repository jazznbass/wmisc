#' Helps merging multilevel datasets
#'
#' @param dat.l1 A data Frame for Level 1
#' @param dat.l2 A data frame for Level 2
#' @param id Stirng with variable name for matching datasets
#' @param var.mean Vector with strings of L1 variables that are aggregated as Level2 means
#'
#' @return A data frame
#' @export

merge_multilevel <- function(dat.l1, dat.l2 = NULL, id, var.mean) {
  names.l1 <- names(dat.l1)
  dat.means <- aggregate(dat.l1[, var.mean], by = list(dat.l1[[id]]), FUN = mean, na.rm = TRUE)
  names(dat.means)[1] <- id
  dat.l1 <- merge(dat.l1, dat.means, by = id, suffix = c("", ".mean"))
  if (is.null(dat.l2)) {
    out <- dat.l1
  } else {
    out <- merge(dat.l1, dat.l2, by = id, suffix = c(".l1", ".l2"))
  }
  out
}
