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
#' @param x A data frame or a vector.
#' @param value Either a single character string when `x` is a vector or a named
#'   list (`columnname = label`) with character strings when `x` is a
#'   data.frame.
#' @rdname rename_from_labels
#' @return For add_label: A vector or a data.frame with added label
#'   attribute(s).
add_label <- function(x, value) {
  
  on.exit(print_messages())
  if (!inherits(value, "list") && length(values) > 0) values <- as.list(values)
  
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

#' Various functions that add and retrieve labels for variabes
#'
#' @description These functions work with an attribute `label`. Which has been
#' introduced by other packages (e.g. `haven`) and is implemented into the
#' R-Studiio environment.
#'
#' @param data A data.frame.
#' @param keep If TRUE, new names are a combination of var names and label
#'   names.
#' @param pattern Pattern for gleu function when `keep = TRUE`.
#' @return For rename_from_labels: A data.frame with renamed labels or for
#'   get_labels a character vector with label names.
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
#' @return For `get_labels`: A names character vector with labels (and columnnames
#'   as names).
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
