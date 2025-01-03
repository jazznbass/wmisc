#' @export
nice_table <- function(object, ...) {
  UseMethod("nice_table")
}

#' @export
nice_table.lm <- nice_regression_table

#' @export
nice_table.lme <- nice_regression_table

#' @export
nice_table.lmer <- nice_regression_table

#' @export
nice_table.lmerModLmerTest <- nice_regression_table

#' @export
nice_table.glmerMod <- nice_regression_table

#' @export
nice_table.lmerMod <- nice_regression_table

#' @export
nice_table.gls <- nice_regression_table

#' @export
nice_table.fa <- nice_efa

#' @export
nice_table.lavaan <- nice_sem

#' @export
nice_table.matrix <- function(object, ...) {
    object <- as.data.frame(object, optional = TRUE)
    nice_table(object, rownames = TRUE, ...)
}




