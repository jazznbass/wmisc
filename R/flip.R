#' Flip a Data Frame or Matrix
#'
#' This function takes a data frame or matrix and flips its rows and columns,
#' effectively transposing the data. Optionally, it can include row names in the 
#' resulting data frame.
#'
#' @param x A data frame or matrix to be flipped.
#' @param rownames Logical. If \code{TRUE}, the resulting data frame will have
#'   row names (if \code{x} is a matrix) or row names will be included as the 
#'   first column (if \code{x} is a data frame).
#' 
#' @return A data frame with flipped rows and columns. If \code{rownames} is 
#'   \code{TRUE}, row names (if \code{x} is a matrix) or the first column will 
#'   contain the original row names (if \code{x} is a data frame).
#' 
#' @export
#' @examples
#' # Create a sample data frame
#' df <- data.frame(A = 1:3, B = 4:6, C = 7:9)
#' 
#' # Flip the data frame
#' flipped_df <- flip(df)
#' 
#' # Flip the data frame with row names included
#' flipped_df_with_rownames <- flip(df, rownames = TRUE)
#' 
#' # Create a sample matrix
#' mat <- matrix(1:9, nrow = 3)
#' 
#' # Flip the matrix
#' flipped_mat <- flip(mat)
#' 
#' # Flip the matrix with row names included
#' flipped_mat_with_rownames <- flip(mat, rownames = TRUE)
#'
#' @seealso \code{\link{t}} for transposing matrices and data frames.
flip <- function(x, rownames = FALSE) {
  if (rownames) x <- cbind(" " = rownames(x), x)
  x <- as.data.frame(t(x))
  x <- setNames(x, x[1, ]) 
  x <- x[-1, ]
  x <- cbind(" " = rownames(x), x)
  rownames(x) <- NULL
  x
}
