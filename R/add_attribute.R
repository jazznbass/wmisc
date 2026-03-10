#' Get or Set 'wmisc' Attributes
#' 
#' These functions allow you to get or set custom attributes under the "wmisc" namespace for any R object.
#' 
#' @param x An R object from which to get or to which to set attributes.
#' @param ... Named attributes to set for the object.
#' #' @return
#' For `get_wmisc_attributes`, a list of attributes stored under "wmisc".
#' For `set_wmisc_attributes`, the original object with updated "wmisc" attributes
#' @keywords internal
get_wmisc_attributes <- function(x) {
  attr(x, "wmisc")
}

#' @rdname get_wmisc_attributes
set_wmisc_attributes <- function(x, ...) {
  args <- attr(x, "wmisc")
  args <- c(args, list(...))
  args <- args[!duplicated(names(args))]
  attr(x, "wmisc") <- args
  x
}


