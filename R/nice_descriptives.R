#' Table with descriptive statistics
#'
#' @param data A data frame
#' @param use_col_labels If TRUE, variable names are taken from a label 
#'  attribute.
#' @param title Title for the table.
#' @param footnote Footnote for the table.
#' @param file If a file name is provided, the table is saved to this file.
#' @param show_scale If TRUE, a column with the scale range is added if the
#'  variable has value labels (either from haven or scaledic attributes).
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
                              show_scale = TRUE,
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
  
  data <- scaledic_to_haven(data)
  if (use_col_labels) data <- rename_from_labels(data)
  
  
  
  
  cols <- c("Variable", "Valid", "Missing", "Mean", "SD", "Min", "Max", "Range", "Median", "MAD")
  
  if (show_scale) {
    has_labels <- any(lapply(data, function(x) length(attr(x, "labels") > 2)) |> unlist())
    if (has_labels) cols <- c(cols, "Scale")
  }
    
  out <- matrix(NA, nrow = ncol(data), ncol = length(cols))
  colnames(out) <- cols
  out <- as.data.frame(out)
  for (col in seq_along(data)) {
    out[col, "Variable"] <- names(data)[col]
    out[col, "Valid"] <- sum(!is.na(data[[col]]))
    out[col, "Missing"] <- sum(is.na(data[[col]]))
    out[col, "Mean"] <- mean(data[[col]], na.rm = TRUE)
    out[col, "SD"] <- sd(data[[col]], na.rm = TRUE)
    out[col, "Min"] <- min(data[[col]], na.rm = TRUE)
    out[col, "Max"] <- max(data[[col]], na.rm = TRUE)
    out[col, "Range"] <- max(data[[col]], na.rm = TRUE) -
      min(data[[col]], na.rm = TRUE)
    out[col, "Median"] <- median(data[[col]], na.rm = TRUE)
    out[col, "MAD"] <- mad(data[[col]], na.rm = TRUE)
  
    if (show_scale && has_labels) {
      lab <- attr(data[[col]], "labels")
      if (length(lab) > 1) {
        sep <- if (length(lab) == 2) "and" else "to" 
        if (!is.null(names(lab))) {
          lab <- paste0(
            lab[1], " (", names(lab)[1] ,") ", sep, " ", 
            lab[length(lab)], " (", 
            names(lab)[length(lab)], ")", collapse = " "
          )  
        } else {
          lab <- paste0(
            lab[1], " to ", 
            lab[length(lab)]
          )  
        }
        out[col, "Scale"] <- lab
      } else {
        out[col, "Scale"] <- ""
      }
    }
  }
  
  out <- data.frame(out)
  rownames(out) <- NULL
 
  out <- set_wmisc_attributes(out, 
    title = title,
    footnote = footnote,
    file = file
  )
  
  nice_table(out, ...)
}
