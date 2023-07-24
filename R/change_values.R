#' Change Values
#'
#' Replace specific values in a vector with new values.
#'
#' @param x A vector in which values will be replaced.
#' @param ... An ellipsis of pairs of values to be replaced. Each pair consists
#'   of the original value and the corresponding replacement value.
#' @param .default The default value to be used for elements not specified in
#'   the replacement pairs. If not provided, elements not specified will remain
#'   unchanged.
#'
#' @return A modified vector with replaced values.
#'
#' @examples
#' change_values(c(1, 2, 3), 2 ~ "two", 3 ~ "three")
#' # Output: c(1, "two", "three")
#'
#' change_values(c(1, 2, 3), 2 ~ "two", .default = "default")
#' # Output: c("default", "two", "default")
#'
#' change_values(c(1, 2, 3), 4 ~ "four")
#' # Output: c("1", "2", "3")
#'
#' @export
change_values <- function(x, ..., .default = NULL) {
  recodes <- lapply(list(...), \(.) list(.[[2]], .[[3]]))
  out <- x
  if (!is.null(.default)) out[] <- rep(.default, length.out = length(x))
  for(i in seq_along(recodes)) {
    out[which(x %in% eval(recodes[[i]][[1]]))] <- eval(recodes[[i]][[2]])
  }
  out
}
