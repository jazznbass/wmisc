#' Recode Values in a Vector
#'
#' Replaces specific values in a vector with new values, using formula-style syntax.
#'
#' @param x A vector whose values will be recoded.
#' @param ... One or more replacement rules in the form `old ~ new`. Each rule specifies a value in `x` to be replaced.
#' @param .default Optional. A default value assigned to all elements not explicitly matched in the replacement rules. If `NULL` (default), unmatched values remain unchanged.
#'
#' @return A vector with values replaced according to the specified rules. If `x` is numeric and at least one replacement is a character string, the result will be coerced to character.
#'
#' @examples
#' # Basic recoding
#' change_values(c(1, 2, 3, NA), 2 ~ "two", 3 ~ "three")
#'
#' # Assign a default value to unmatched entries
#' change_values(c(1, 2, 3), 2 ~ "two", .default = "default")
#'
#' # Unmatched values remain unchanged (no warning or error)
#' change_values(c(1, 2, 3), 4 ~ "four")
#'
#' # Recode missing values (NA)
#' change_values(c(NA, 1, NA, 2), NA ~ "This is a missing value")
#'
#' @seealso [recode()], [ifelse()], [case_when()]
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

