#' Format p values
#'
#' @param p Vector of p values.
#' @param equal_sign If TRUE an equal sign is set before p values.
#'
#' @return A character vector with nicely formatted p values
#' @export
#' @examples 
#' p <- c(0.04, 0.9, 0.10, 0.001, 1, NA)
#' nice_p(p)
#' paste0("p", nice_p(p, equal_sign = TRUE))
nice_p <- function(p, equal_sign = FALSE, digits = 3, stars = FALSE) {
  unlist(
    lapply(p, .nice_p, equal_sign = equal_sign, digits = digits, stars = stars)
  )
}

.nice_p <- function(p, equal_sign, digits, stars) {
  if (is.na(p)) return(NA)
  equal_sign <- if (equal_sign) "=" else ""
  if (isTRUE(p >= 0.05)) {
    return(paste0(equal_sign, nice_statnum(p, digits = digits)))
  }
  if (isTRUE(p < 0.001)) {
    if (!stars) {
      return("<.001") 
    } else {
      return("<.001***") 
      #return(paste0(equal_sign, nice_statnum(p, digits = digits), "***"))
    }

  }
  if (isTRUE(p < 0.01)) {
    if (!stars) {
      return("<.01") 
    } else {
      return(paste0(equal_sign, nice_statnum(p, digits = digits), "**"))
    }
  }
  if (isTRUE(p < 0.05)) {
    if (!stars) {
      return("<.05") 
    } else {
      return(paste0(equal_sign, nice_statnum(p, digits = digits), "*"))
    }
  }
}


