#' Flip a Data Frame or Matrix
#'
#' This function takes a data frame or matrix and flips its rows and columns,
#' effectively transposing the data. Optionally, it can include row names in the 
#' resulting data frame.
#'
#' @param x A data frame or matrix to be flipped.
#' @param rownames Logical or string. If `TRUE`, row names will be included as the 
#'   first column. If `character`, the column will be named respectively.
#' 
#' @return A data frame with flipped rows and columns. If \code{rownames} is 
#'   \code{TRUE}, row names (if `x` is a matrix) or the first column will 
#'   contain the original row names (if `x` is a data frame).
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
#' @seealso \code{\link{t}} for transposing matrices and data frames.
flip <- function(x, rownames = NULL) {
  if (isTRUE(rownames)) x <- cbind(" " = rownames(x), x)
  if (is.character(rownames)) {
    rn <- data.frame(x = rownames(x))
    names(rn) <- rownames
    x <- cbind(rn, x)
  }
  x <- as.data.frame(t(x))
  x <- setNames(x, x[1, ]) 
  x <- cbind(" " = rownames(x), x)
  names(x)[1] <- x[1, 1]
  x <- x[-1, ]
  rownames(x) <- NULL
  x
}
