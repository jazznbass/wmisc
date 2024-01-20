#' @export
add_attribute <- function(x, attribute, value) {
  attr(x, paste0("wmisc_", attribute)) <- value
  x
}

#' @export
add_title <- function(x, value) {
  add_attribute(x, "title", value)
}

#' @export
add_note <- function(x, value) {
  add_attribute(x, "note", value)
}
