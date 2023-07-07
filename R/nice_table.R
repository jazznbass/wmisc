#' Create a nicely formatted table
#' 
#' This function takes a data frame and formats it into a nicely
#' formatted HTML table using the `knitr` and `kableExtra` packages.
#' 
#' @param x The data frame to be formatted into a table
#' @param ... Additional arguments passed to `knitr::kable()`
#' @param extra Additional arguments passed to `kableExtra::kable_classic_2()`
#' @param title Title string.
#' @param footnote Add footnote
#' @return A nicely formatted HTML table
#' 
#' 
#' @examples
#' df <- data.frame(x = 1:5, y = rnorm(5))
#' nice_table(df, extra = list(full_width = FALSE))
#' 
#' @export
nice_table <- function(x, ..., extra = NULL, title = "", footnote = "") {
  title <- paste0("Table.<br><i>", title, "</i>")
  x <- knitr::kable(x, caption = title, ...)
  do.call(kableExtra::kable_classic, c(list(x), extra)) %>%
    kableExtra::footnote(footnote)
}
