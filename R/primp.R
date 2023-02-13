#' primp
#'
#' Get styled console text output
#'
#' Possible styles are: "bold", "faint", "italic", "underline", "swap",
#' "strike", "black", "red", "green", "yellow", "blue", "magenta", "cyan",
#' "white", "lightblack", "lightred", "lightgreen", "lightyellow", "lightblue",
#' "lightmagenta", "lightcyan", "lightwhite", "bg-black", "bg-red", "bg-green",
#' "bg-yellow", "bg-blue", "bg-magenta", "bg-cyan", "bg-white", "bg-lightblack",
#' "bg-lightred", "bg-lightgreen", "bg-lightyellow", "bg-lightblue",
#' "bg-lightmagenta", "bg-lightcyan", "bg-lightwhite"
#'
#' @param x A text containing possible styles in curly brackets, separated by
#'   comma, semicolon or space.
#' @param cat TRUE or FALSE
#' @return Cat a string or returns string
#' @export
#'
#' @examples
#' primp("{green bold}Jo Lady, {blue,italic}that's me, {bg-red}in a tree!")
#' @export
primp <- function(x, cat = TRUE) {
  
  styles <- c(black = 30, red = 31, green = 32, yellow = 33, 
              blue = 34, magenta = 35, cyan = 36, white = 37)
  styles <- c(styles, setNames(90:97, paste0("light", names(styles))))
  styles <- c(styles, setNames(c(40:47, 100:107), paste0("bg-", names(styles))))
  styles <- c(styles, bold = 1, faint = 2, italic = 3, underline = 4, swap = 7,
              strike = 9)
  
  start <- unlist(gregexpr('\\{', x))
  stop <- unlist(gregexpr('\\}', x))
  n_styles <- length(start)
  
  args <- character(n_styles)
  for(i in seq_along(start)) {
    extract <- substr(x, start[i]+1, stop[i]-1)
    extract <- unlist(strsplit(extract, "[,; ]"))
    extract <- sapply(extract, function(x) {
      .cset(setNames(styles[which(trimws(x) == names(styles))], NULL))
    })
    args[i] <- paste0(extract, collapse = "")
  }
  
  out <- c(substr(x, 0, start[1]-1), args[1])
  for(i in 2:n_styles) {
    out <- c(out, substr(x, stop[i-1]+1, start[i]-1), .cset(), args[i])
  }
  
  out <- c(out, substr(x, stop[n_styles]+1, nchar(x)), .cset())
  out <- paste0(out, collapse = "")
  if (cat) cat(out) else out
}

.cset <- function(x) {
  if (missing(x)) x <- "0" 
  paste0("\033[", x, "m")
}
