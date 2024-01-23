#' @export
get_wmisc_attributes <- function(x) {
  attr(x, "wmisc")
}

#' @export
set_wmisc_attributes <- function(x, ...) {
  args <- attr(x, "wmisc")
  args <- c(args, list(...))
  args <- args[!duplicated(names(args))]
  attr(x, "wmisc") <- args
  x
}

#' @export
set_wmisc_attribute <- function(x, attribute, value) {
  args <- attr(x, "wmisc")
  args[[attribute]] <- value
  attr(x, "wmisc") <- args
  x
}

#' @export
get_wmisc_attribute <- function(x, attribute) {
  attr(x, "wmisc")[[attribute]]
}

#' @export
set_title <- function(x, value) {
  set_wmisc_attribute(x, "title", value)
}

#' @export
get_title <- function(x) {
  get_wmisc_attribute(x, "title")
}

#' @export
set_note <- function(x, value) {
  set_wmisc_attribute(x, "note", value)
}

#' @export
get_note <- function(x) {
  get_wmisc_attribute(x, "note")
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
