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
#' ## Retains NAs
#' change_values(c(1, 2, 3, NA), 2 ~ "two", 3 ~ "three")
#' 
#' ## Set a specific default value
#' change_values(c(1, 2, 3), 2 ~ "two", .default = "default")
#' 
#' ## No error message when no value to convert is found
#' change_values(c(1, 2, 3), 4 ~ "four")
#' 
#' ## Recode NAs and implicit conversion from numeric to character
#' change_values(c(NA, 1, NA, 2), NA ~ "This is a missing value")
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

change_by_list <- function(x, rn) {
  for(i in seq_along(rn)) {
    x[which(x %in% names(rn)[i])] <- rn[i]
  }
  x
}

