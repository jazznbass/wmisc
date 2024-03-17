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
  if (!inherits(value, "list") && length(value) > 1) value <- as.list(value)
  
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
#' @param max_char Maximum number of characters for the label.
#' @return For rename_from_labels: A data.frame with renamed labels or for
#'   get_labels a character vector with label names.
#' @export
rename_from_labels <- function(data, 
                               keep = FALSE,
                               pattern = "{name}: {label}",
                               max_char = NULL) {
  ne <- new.env(parent = globalenv())
  for(i in seq_along(data)) {
    if (!is.null(attr(data[[i]], "label"))) {
      if (!keep) names(data)[i] <- attr(data[[i]], "label")
      if (keep) {
        ne$label <- attr(data[[i]], 'label')
        ne$name <- names(data)[i]
        new_name <- glue::glue(pattern, .envir = ne) 
        if (!is.null(max_char)) new_name <- substr(new_name, 1, max_char)
        names(data)[i] <- new_name
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
                       pattern = "{name}: {label}",
                       max_char = NULL) {
  out <- lapply(data, \(.) attr(., "label")) |> unlist()

  if (keep) {
    ne <- new.env(parent = globalenv())
    ne$label <- out
    ne$name <- names(out)
    new_name <- glue::glue(pattern, .envir = ne) |> as.character()
    if (!is.null(max_char)) new_name <- substr(new_name, 1, max_char)
    out <- setNames(new_name, names(out))
  }
  out
}
