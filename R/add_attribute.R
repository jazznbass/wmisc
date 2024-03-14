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


