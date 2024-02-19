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
  
  on.exit(print_messages())
  
  if (inherits(value, "list")) {
    for(i in seq_along(value)) {
      variable <- names(value)[i]
      if (is.null(x[[names(value)[i]]])) {
        add_message("'", variable, "' does not exist.")
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
rename_from_labels <- function(data, keep = FALSE) {
  for(i in seq_along(data)) {
    if (!is.null(attr(data[[i]], "label"))) {
      if (!keep) names(data)[i] <- attr(data[[i]], "label")
      if (keep) names(data)[i] <- glue("{names(data)[i]}: {attr(data[[i]], 'label')}") 
    }
  }
  data
}

#' @export
get_labels <- function(data) {
  lapply(data, \(x) attr(x, "label")) |> unlist()
}
