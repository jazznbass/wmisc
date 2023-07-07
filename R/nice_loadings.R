#' Creates table of loadings of exploratory factor analysis based on the
#' psych::fa function
#'
#' @param object Object returned from the psych::fa function.
#' @param factor_names A character vector with names for the resulting factors.
#'   If not provided, default names are chosen.
#' @param sort If TRUE, loadings are sorted.
#' @param cut Loadings below cut will be omitted in the resulting data frame.
#' @param round Number of digits to round loadings (based on the base::round
#'   function)
#' @return A data.frame
#' @export

nice_loadings <- function(object,
                          factor_names = NULL,
                          sort = TRUE,
                          cut = 0.2,
                          round = 2) {
  
  var_exp <- object$Vaccounted
  if(sort) object <- psych::fa.sort(object)
  object <- loadings(object)
  object <- unclass(object)
  object <- round(object, round)
  object[abs(object) < cut] <- ""
  object <- as.data.frame(object)
  if (!is.null(factor_names)) names(object) <- factor_names[1:ncol(names)]
  object <- rbind(object, round(var_exp, round))
  object
}

