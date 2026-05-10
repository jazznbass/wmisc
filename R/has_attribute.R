#' Checks for attributes
#'
#' This function takes an object returns a logical vector of the same length as
#' the number of columns in the object, where each element is TRUE if the
#' corresponding column has the specified attribute values and FALSE otherwise.
#'
#' @param .x A dataframe, a list or a single object. If a dataframe is provided,
#'   the function will check the attributes of each column. If a list is
#'   provided, the function will check the attributes of each element. If a 
#'   single object is provided, the function will check the attributes of that 
#'   object.
#' @param expr A logical expression based on the attributes of the object(s).
#'  The expression should be written in terms of the attributes of the object(s).
#'  For example, if you want to check for an attribute called "my_attr" that 
#'  should be equal to "my_value", you would write the expression as `my_attr == "my_value"`.
#' @param dic A logical value indicating whether to look directly and only 
#'  `scaledic` attributes.
#' @return A logical vector indicating which columns (or elements) have the 
#'  specified attribute. 
#' @export
has_attribute <- function(.x, expr, dic = FALSE) {
  expr <- substitute(expr)
  if (!is.list(.x)) .x <- list(.x)
  vapply(.x, function(x) {
    a <- attributes(x)
    if (dic) a <- a$dic
    tryCatch(!is.null(a) && isTRUE(eval(expr, envir = a)), error = function(e) FALSE)
  }, logical(1))
}

