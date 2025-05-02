#' Round Numeric Columns in a Data Frame
#'
#' Rounds all numeric columns of a data frame to a specified number of digits.
#' Non-numeric columns and attributes are preserved.
#'
#' @param x A data frame.
#' @param digits Integer. Number of digits to round to.
#'
#' @return A data frame with all numeric columns rounded to the specified number of digits.
#'
#' @examples
#' data <- data.frame(
#'   numeric_col1 = c(1.234, 2.345, 3.456),
#'   numeric_col2 = c(4.567, 5.678, 6.789),
#'   non_numeric_col = c("A", "B", "C")
#' )
#' attr(data, "note") <- "It keeps all the attributes!"
#' rounded <- round_numeric(data, digits = 2)
#' rounded
#' attr(rounded, "note")
#'
#' @seealso [round()]
#'
#' @export
round_numeric <- function(x, digits) {
  if (is.null(digits)) return(x)
  id <- lapply(x, function(.) is.numeric(.)) |> unlist() |> which()
  x[, id] <- round(x[, id], digits)
  x
}
