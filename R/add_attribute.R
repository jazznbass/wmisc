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
      variable <- names(value)[i]
      if (is.null(x[[names(value)[i]]])) {
        warning("'", variable, "' does not exist.\n", call. = FALSE)
        next
      }
      
      attr(x[[names(value)[i]]], "label") <- value[[i]]
    }
  } else {
    attr(x, "label") <- value
  }
  x
}

#' Renames the variables from a data frame to a label that is provided in the label attribute
#' @param data A dataframe
#' @return A dataframe wit renamed labels
#' @export
rename_from_labels <- function(data) {
  for(i in seq_along(data)) {
    if (!is.null(attr(data[[i]], "label"))) names(data)[i] <- attr(data[[i]], "label")
  }
  data
}
