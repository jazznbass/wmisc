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
#' wmisc:::mtcars_labeled |> 
#'   rename_from_labels() |>
#'   psych::fa(nfactors = 2) |> 
#'   nice_efa()
#' 
#' nice_efa(
#'   wmisc:::data_emo_fa,
#'   factor_names = c(
#'     "Emotionserkennung", "Resilienz", 
#'     "Aufmerksamkeit", "Erklärung für Emotionen"
#'   )
#' )

#' @export

nice_loadings <- function(x,
                          factor_names = NULL,
                          sort = TRUE,
                          cut = 0.2,
                          round = 2,
                          title = "Loading matrix",
                          footnote = NULL,
                          auto_labels = TRUE){
  
  if (!inherits(x, "psych")) 
    stop("Object must be derived from the factor analyses of the psych package.")
  
  object <- x
  var_exp <- object$Vaccounted
  if(sort) x <- psych::fa.sort(x)
  object <- loadings(x)
  object <- unclass(object)
  
  complexity <- x$complexity
  communalities <- 1 - x$uniquenesses
  object <- cbind(
    object, 
    Communalities = communalities, 
    Complexity = complexity
  )
  
  object <- round(object, round)
  object[abs(object) < cut] <- ""
  object <- as.data.frame(object)
  
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
  if (!is.null(factor_names)) 
    names(object)[2:(1 + length(factor_names))] <- factor_names
  
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
  
  rn <- c("SS loadings" = "Eigenvalues", "Proportion Var" = "Explained variance",
          "Cumulative Var" = "Cumulative explained variance",
          "Proportion Explained" = "Proportion explained variance",
          "Cumulative Proportion" = "Cumulative proportion explained variance")
  counter <- 0
  for (i in seq_along(rn)) {
    id <- which(out[[1]] == names(rn)[i])
    if (length(id) > 0)  {
      out[id, 1] <- rn[i]
      counter <- counter + 1
    }
  }
  
  out <- set_wmisc_attributes(
    out, 
    spanner = list("Factors" = 2:(ncol(out) - 2)),
    row_group = list(
      "Loadings" = 1:(rows - counter), 
      "Variances" = (rows - counter + 1):rows
    ),
    file = file
  )
  
  nice_table(out)
}

