#' Checks arguments and returns a single error message for all errors.
#' 
#' @param ... 
#'
#' @export
check_args <- function(...) {
  expressions <- substitute(list(...))
  
  env <- new.env(parent = parent.frame()) 
  env$equal <- function(condition, ...) {
    message <- paste0(...)
    if (length(message) == 0) 
      message <- paste0(as.character(match.call()[2]), " failed.")
    if (isFALSE(condition)) return(message) else return(TRUE)
  }
  env$not <- function(condition, ...) env$equal(!condition, ...)
  env$one_of <- function(arg, ...) {
    match <- c(...)
    msg <- paste0("'", match, "'")
    if (length(match) == 2) msg <- paste0(msg, collapse = " or ")
    if (length(match) > 2) msg <- paste0("one of ", paste0(msg, collapse = ", "))
    env$equal(
      arg %in% match, 
      as.character(match.call()[2]), " is not ", msg, "."
    )
  }
  
  env$within <- function(arg, lower, upper) {
    if (!missing(lower))
      x <- env$not(
        any(unlist(arg) < lower), as.character(match.call()[3]), " < ", lower
      ) 
    if (!missing(upper))
      x <- env$not(
        any(unlist(arg) > upper), as.character(match.call()[3]), " > ", upper
      ) 
    x
  }
  
  env$class <- function(param, class, ...) {
    env$equal(
      inherits(param, class), 
      as.character(match.call()[2]), " is not of class ", class, "."
    )
  }
  
  out <- vector("list", length(expressions) - 1)
  for(i in 2:length(expressions)) {
    out[i - 1] <- eval(expressions[c(1, i)], envir = env)
    
  }
  out <- out[sapply(out, function(x) if (!isTRUE(x)) TRUE else FALSE)]
  
  if (length(out) > 0) {
    out <- paste0(1:length(out), ": ", unlist(out), "\n")
    stop("\n", out, call. = FALSE)
  }
}
