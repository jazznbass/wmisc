#' Read a single row of data into a data frame
#' 
#' This function takes a character string representing a single row of data,
#' with columns separated by a specified separator, and converts it into a data
#' frame.
#' 
#' @keywords internal
#' @param x A character string representing a row of data
#' @param sep The separator used in the data string. Default is "|" 
#' @return A data frame with one row
#' @examples
#' row_df("id|name|age\n1|Alice|30")
#' row_df("id,name,age\n3,Charlie,35", sep = ",")
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
