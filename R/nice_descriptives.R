#' Table with descriptive statistics
#'
#' @param data A data frame
#' @param use_col_labels If TRUE, variable names are taken from a label 
#'  attribute.
#' @param title Title for the table.
#' @param footnote Footnote for the table.
#' @param file If a file name is provided, the table is saved to this file.
#' @param ... Further arguments passed to [nice_table()].
#' @return A data frame with descriptive statistics
#' @examples
#' nice_descriptives(mtcars)
#' 
#' nice_descriptives(mtcars_labeled, auto_labels = TRUE, round = 2)
#' @export
nice_descriptives <- function(data, 
                              use_col_labels = TRUE,
                              title = "Descriptive statistics",
                              footnote = NULL,
                              file = NULL,
                              ...) {
  
  ## init_messages(); on.exit(print_messages())
  
  if (is.null(footnote)) 
    footnote <- paste0("MAD is the median average deviation with a ",
                       "consistency adjustment")
  

  .logical <- sapply(data, is.logical)
  if (any(.logical)) {
    for (i in seq_along(data)) {
      if (.logical[i]) data[[i]] <- as.numeric(data[[i]])
    }
    notify(
      "Some variables were converted from logical to numeric: ",
      paste0(names(data)[.logical], collapse = ", ")
    )
  }
  
  .factor <- sapply(data, is.factor)
  if (any(.factor)) {
    for (i in seq_along(data)) {
      if (.factor[i] && nlevels(data[[i]]) == 2) {
        notify(
          "Variable '", names(data)[i] ,"' converted from factor to numeric (0/1)"
        )
        names(data)[i] <- paste0(names(data)[i], " (", levels(data[[i]])[2],")")
        .label <- attr(data[[i]], "label") 
        if (!is.null(.label))          
          attr(data[[i]], "label") <- paste0(.label, " (", levels(data[[i]])[2],")") 
        data[[i]] <- as.numeric(data[[i]]) - 1
        
      }
    }
  }
  
  .filter <- sapply(data, is.numeric)
  if (any(!.filter)) {
    notify(
      "Some variables are not numeric and dropped from the analysis: ",
      paste0(names(.filter)[!.filter], collapse = ", ")
    )
  }
  
  data <- data[, .filter]
  
  if (use_col_labels) data <- rename_from_labels(data)
  
  out <- apply(data, 2, function(x)
    c(
      Valid = sum(!is.na(x)),
      Missing  = sum(is.na(x)),
      Mean = mean(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE),
      Min = min(x, na.rm = TRUE),
      Max = max(x, na.rm = TRUE),
      Range = max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
      Median = median(x, na.rm = TRUE),
      MAD = mad(x, na.rm = TRUE)
    ))
  
  out <- t(out)
  out <- data.frame(out)
  out <- cbind(Variable = rownames(out), out)
  rownames(out) <- NULL
 
  out <- set_wmisc_attributes(out, 
    title = title,
    footnote = footnote,
    file = file
  )
  
  nice_table(out, ...)
}
