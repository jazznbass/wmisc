#' This function rounds numeric columns in a data frame to a specified number of digits.
#'
#' @param x A data frame.
#' @param digits Number of digits to round to. Defaults to 0.
#'
#' @return A modified data frame with rounded numeric columns.
#'
#' @examples
#' # Create a sample data frame
#' data <- data.frame(
#'   numeric_col1 = c(1.234, 2.345, 3.456),
#'   numeric_col2 = c(4.567, 5.678, 6.789),
#'   non_numeric_col = c("A", "B", "C")
#' )
#' attr(data, "my") <- "It keeps all the attributes!"
#' # Round numeric columns in the data frame
#' round_numeric(data, digits = 2)
#' attr(data, "my") 
#' @seealso \code{\link{round}}
#'
#' @export
round_numeric <- function(x, digits = 0) {
  id <- lapply(x, \(.) is.numeric(.)) |> unlist() |> which()
  x[, id] <- round(x[, id], digits)
  x
}
