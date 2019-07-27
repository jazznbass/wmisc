#' Show class of all cols in a data.frame
#'
#' @param data A data.frame
#'
#' @return A data.frame with variable names and classes

x_class <- function(data) {
  data.frame(CLASS = unlist(lapply(data, class)))
}
