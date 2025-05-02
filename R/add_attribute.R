#' Access or Modify "wmisc" Attributes
#'
#' These functions access or modify the `"wmisc"` attribute of an R object.
#' The attribute is typically used to store metadata in `wmisc`-compatible structures.
#'
#' @param x An R object (usually a list) from or to which the `"wmisc"` attribute is accessed or modified.
#' @param ... Named values to add or update in the `"wmisc"` attribute.
#'
#' @return
#' `get_wmisc_attributes()` returns the `"wmisc"` attribute of `x`, or `NULL` if none is present.  
#' `set_wmisc_attributes()` returns `x` with the updated `"wmisc"` attribute.
#'
#' @details
#' - `get_wmisc_attributes()` extracts and returns the `"wmisc"` attribute. Returns `NULL` if it does not exist.  
#' - `set_wmisc_attributes()` adds or updates entries in the `"wmisc"` attribute. If a name occurs multiple times, the last value is retained.
#'
#' @examples
#' obj <- list(a = 1, b = 2)
#' obj <- set_wmisc_attributes(obj, key1 = "value1", key2 = "value2")
#' get_wmisc_attributes(obj)
#' obj <- set_wmisc_attributes(obj, key1 = "new_value1", key3 = "value3")
#' get_wmisc_attributes(obj)
#'
#' @seealso [attributes()], [attr()]
#' @export
get_wmisc_attributes <- function(x) {
  attr(x, "wmisc")
}

#' @rdname get_wmisc_attributes
#' @export
set_wmisc_attributes <- function(x, ...) {
  args <- attr(x, "wmisc")
  args <- c(args, list(...))
  args <- args[!duplicated(names(args))]
  attr(x, "wmisc") <- args
  x
}


