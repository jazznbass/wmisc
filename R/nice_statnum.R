#' Nice statnum
#' Formats numbers in the -1 to 1 range
#' 
#' @param x number
#' @param digits Number to round to
#' @export
nice_statnum <- function(x, digits = 2) {
  sub("^(-?)0.", "\\1.", sprintf(paste0("%.", digits, "f"), x))
}
