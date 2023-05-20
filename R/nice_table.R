#' Create a nicely formatted table
#' 
#' This function takes a data frame and formats it into a nicely
#' formatted HTML table using the `knitr` and `kableExtra` packages.
#' 
#' @param x The data frame to be formatted into a table
#' @param ... Additional arguments passed to `knitr::kable()`
#' @param extra Additional arguments passed to `kableExtra::kable_classic_2()`
#' 
#' @return A nicely formatted HTML table
#' 
#' @import knitr
#' @import kableExtra
#' 
#' @examples
#' df <- data.frame(x = 1:5, y = rnorm(5))
#' nice_table(df, extra = list(full_width = FALSE))
#' 
#' @export
nice_table <- function(x, ..., extra = NULL) {
  x <- knitr::kable(x, ...)
  do.call(kableExtra::kable_classic_2, c(list(x), extra))
}
