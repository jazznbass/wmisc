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
#' @param x A text
#' @param ... Style words
#'
#' @return
#' @export
#'
#' @examples
primp <- function(x, ...) {
  
  styles <- c(black = 30, red = 31, green = 32, yellow = 33, 
              blue = 34, magenta = 35, cyan = 36, white = 37)
  styles <- c(styles, setNames(90:97, paste0("light", names(styles))))
  styles <- c(styles, setNames(c(40:47, 100:107), paste0("bg-", names(styles))))
  styles <- c(styles, bold = 1, faint = 2, italic = 3, underline = 4, swap = 7,
              strike = 9)
  
  args <- list(...)
  
  args <- lapply(args, function(x) {
    if(is.numeric(x)) x else setNames(styles[which(x == names(styles))], NULL)
  }
  )
  values <- paste0(args, collapse = ";")
  
  out <- paste0(cset(values),x,cset())
  cat(out)
}
