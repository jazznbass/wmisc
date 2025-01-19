#' Get or Set "wmisc" Attributes
#'
#' Functions to retrieve or modify the "wmisc" attributes of an object.
#'
#' @param x An object, typically a list or data structure, where the "wmisc" attribute is to be accessed or modified.
#' @param ... Additional named arguments to set or update as part of the "wmisc" attribute.
#'
#' @return 
#' - For `get_wmisc_attributes()`: Returns the "wmisc" attribute of the object `x`.
#' - For `set_wmisc_attributes()`: Returns the object `x` with the updated "wmisc" attribute.
#'
#' @details
#' - `get_wmisc_attributes()`: Extracts and returns the "wmisc" attribute of the given object. If the attribute does not exist, it returns `NULL`.
#' - `set_wmisc_attributes()`: Updates the "wmisc" attribute of the given object with the provided named arguments. Duplicate names are removed, with the most recently provided value taking precedence.
#'
#' @examples
#' # Example object
#' obj <- list(a = 1, b = 2)
#'
#' # Set "wmisc" attributes
#' obj <- set_wmisc_attributes(obj, key1 = "value1", key2 = "value2")
#'
#' # Get "wmisc" attributes
#' get_wmisc_attributes(obj)
#'
#' # Update "wmisc" attributes
#' obj <- set_wmisc_attributes(obj, key1 = "new_value1", key3 = "value3")
#' get_wmisc_attributes(obj)
#'
#' @seealso [attributes()], [attr()]
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


