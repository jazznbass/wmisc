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

#' Renames the variables from a data frame to a label that is provided in the
#' label attribute
#' @param data A data.frame.
#' @param keep If TRUE, new names are a combination of var names and label
#'   names.
#' @param pattern Pattern for gleu function when `keep = TRUE`.
#' @return A data.frame with renamed labels or for get_labels a character vector
#'   with label names.
#' @export
rename_from_labels <- function(data, 
                               keep = FALSE,
                               pattern = "{old_name}: {label}") {
  ne <- new.env(parent = globalenv())
  for(i in seq_along(data)) {
    if (!is.null(attr(data[[i]], "label"))) {
      if (!keep) names(data)[i] <- attr(data[[i]], "label")
      if (keep) {
        
        ne$label <- attr(data[[i]], 'label')
        ne$old_name <- names(data)[i]
        out <- glue::glue(pattern, .envir = ne)
        names(data)[i] <- glue::glue(pattern, .envir = ne)
      }
    }
  }
  data
}

#' @export
#' @rdname rename_from_labels
get_labels <- function(data,
                       keep = FALSE,
                       pattern = "{old_name}: {label}") {
  out <- lapply(data, \(.) attr(., "label")) |> unlist()
  if (keep) {
    ne <- new.env(parent = globalenv())
    ne$label <- out
    ne$old_name <- names(out)
    out <- glue::glue(pattern, .envir = ne)
  }
  out
}
