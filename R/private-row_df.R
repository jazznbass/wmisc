#' Read a single row of data into a data frame
#' @param x A character string representing a row of data
#' @param sep The separator used in the data string. Default is "|" 
row_df <- function(x, sep = "|") {
  x <- trimws(x)
  read.table(
    text = x,
    sep = sep,
    header = TRUE,
    quote = "",
    stringsAsFactors = FALSE,
    fill = TRUE,
    strip.white = TRUE,
    comment.char = ""
  )
}
