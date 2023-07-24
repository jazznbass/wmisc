
#' @export
flip <- function(x, rownames = FALSE) {
  if (rownames) x <- cbind(" " = rownames(x), x)
  x <- as.data.frame(t(x))
  x <- setNames(x, x[1, ]) 
  x <- x[-1, ]
  x <- cbind(" " = rownames(x), x)
  rownames(x) <- NULL
  x
}
