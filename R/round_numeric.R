#' Round Numeric Columns in a Data Frame
#'
#' Rounds all numeric columns of a data frame to a specified number of digits.
#' Non-numeric columns and attributes are preserved.
#'
#' @param x A data frame.
#' @param digits Integer. Number of digits to round to. Or a vector with variable names and digits to round.
#'
#' @return A data frame with all numeric columns rounded to the specified number of digits.
#'
#' @examples
#' data <- data.frame(
#'   col1 = c(1.234, 2.345, 3.456),
#'   col2 = c(4.567, 5.678, 6.789),
#'   non_numeric_col = c("A", "B", "C")
#' )
#' round_numeric(data, digits = 2)
#' round_numeric(data, digits = c(col1 = 1, col2 = 2))
#' 
#' @seealso [round()]
#'
#' @export
round_numeric <- function(x, digits) {
  if (is.null(digits)) return(x)
  if (!is.null(names(digits))) {
    for(i in seq_along(digits)) {
      x[, names(digits)[i]] <- round(x[, names(digits)[i]], digits[i])
    }
  } else {
    id <- lapply(x, function(.) is.numeric(.)) |> unlist() |> which()
    x[, id] <- round(x[, id], digits)
    
  }
  x
}
