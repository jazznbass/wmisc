#' Creates table of loadings of exploratory factor analysis based on the
#' psych::fa function
#'
#' @param x Object returned from the psych::fa function.
#' @param factor_names A character vector with names for the resulting factors.
#'   If not provided, default names are chosen.
#' @param sort If TRUE, loadings are sorted.
#' @param cut Loadings below cut will be omitted in the resulting data frame.
#' @param round Number of digits to round loadings (based on the base::round
#'   function)
#' @return A data.frame
#' @examples
#' mtcars |> psych::fa(nfactors = 2) |> nice_efa()
#' 
#' @export

nice_loadings <- function(x,
                          factor_names = NULL,
                          sort = TRUE,
                          cut = 0.2,
                          round = 2,
                          title = "Loading matrix",
                          footnote = NULL,
                          auto_labels = TRUE){
  
  object <- x
  var_exp <- object$Vaccounted
  if(sort) x <- psych::fa.sort(x)
  object <- loadings(x)
  object <- unclass(object)
  
  complexity <- x$complexity#[object$order]
  communalities <- 1-x$uniquenesses#[object$order]
  object <- cbind(object, Communalities = communalities, Complexity = complexity)
  
  object <- round(object, round)
  object[abs(object) < cut] <- ""
  object <- as.data.frame(object)
  if (!is.null(factor_names)) names(object) <- factor_names[1:ncol(names)]
  var_exp <- round(var_exp, round)
  var_exp <- cbind(var_exp, Communalities = "", Complexity = "")
  object <- rbind(object, var_exp)
  object <- cbind("Variables" = rownames(object), object)
  rownames(object) <- NULL
  
  if (is.null(footnote)) {
    footnote <- c(
      paste0("Extraction method is ", x$fm), 
      paste0("Rotation method is ", x$rotation),
      paste0(
        "RMSEA is ", x$RMSEA[1] |> round(3), 
        " CI", round(x$RMSEA[4] * 100), "% [", round(x$RMSEA[2], 3), 
        ", ", round(x$RMSEA[3], 3), "]"
      ),
      if (is.numeric(cut)) paste0(
        "Loadings below |", cut, "| are not displayed"
      )
    )
  }
  
  object <- set_wmisc_attributes(
    object, 
    title = title,
    note = footnote
  )
  
  object
}

#' @rdname nice_loadings
#' @export
nice_efa <- function(..., file = NULL) {
  out <- nice_loadings(...)
  rows <- nrow(out)
  out[(rows - 4):rows, 1] <- c(
    "Eigenvalues", "Explained variance", 
    "Cumulative explained variance",
    "Proportion explained variance",
    "Cumulative proportion explained variance"
  )
  
  out <- set_wmisc_attributes(
    out, 
    spanner = list("Factors" = 2:(ncol(out) - 2)),
    row_group = list(
      "Loadings" = 1:(rows - 5), 
      "Variances" = (rows - 4):rows
    ),
    file = file
  )
  
  nice_table(out)
}

