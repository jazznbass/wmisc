#' Show class of all cols in a data.frame
#'
#' @param data A data.frame
#' 
#' @return A data.frame with variable names and classes

list_class <- function(data) {
  data.frame(class = unlist(lapply(data, class)))
}
