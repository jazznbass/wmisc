#' Moving Average
#'
#' @param x Vector
#' @param xLag Lag for calculating the average
#' @param FUN Function for calculating the average (e.g., FUN = mean)
#'
#' @export

moving_average <- function(x, xLag, FUN = mean) {
  x <- as.vector(x, mode = "numeric")
  if (length(x) < xLag * 2 + 1) return(NA)
  for (i in (xLag + 1):(length(x) - xLag)) {
    x[i] <- FUN(x[(i - xLag):(i + xLag)], na.rm = TRUE)
  }
  x
}
