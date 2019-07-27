#' x.names
#'
#' Give a list of variable names to the CLIPBOARD and the screen
#'
#' @param A data-frame or another object of class list
#'
#' @return A character string with variable names formated as "c(naem, name, name, ...)"
#' @export

x_names <- function(list, width = 70) {
  list <- names(list)
  b <- paste("\"", list, "\", ", sep = "", collapse = "")
  a <- paste("c(", substr(b, start = 1, stop = nchar(b) - 2), ")", sep = "", collapse = "")
  signs <- which(strsplit(a, "")[[1]] == ",")
  width2 <- width
  for (i in 1:length(signs)) {
    if (signs[i] >= width2) {
      lhs <- substring(a, 1, signs[i] + 1)
      rhs <- substring(a, signs[i] + 2)
      a <- paste0(lhs, "\n  ", rhs)
      width2 <- width + signs[i] + 3
      signs <- which(strsplit(a, "")[[1]] == ",")
    }
  }

  # clip <- pipe("pbcopy", "w")
  cat(a) # , file = clip)
  # close(clip)
  cat(a, "\r\n")
}
