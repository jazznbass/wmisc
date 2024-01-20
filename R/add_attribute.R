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

#' @export
add_label <- function(x, value) {
  
  if (inherits(value, "list")) {
    for(i in seq_along(value)) {
      attr(x[[names(value)[i]]], "label") <- value[[i]]
    }
  } else {
    attr(x, "label") <- value
  }
  x
}

