#' Table with descriptive statistics
#'
#' @param data A data frame
#' @param round Digits for round function
#' @param auto_labels If TRUE, variable names are taken from a label attribute.
#'
#' @return A data frame with descriptive statistics
#' @examples
#' nice_descriptives(mtcars)
#' 
#' nice_descriptives(wmisc:::mtcars_labeled, auto_labels = TRUE, round = 2)
#' @export
nice_descriptives <- function(data, 
                              round = NULL, 
                              auto_labels = FALSE,
                              title = "Descriptive statistics",
                              footnote = NULL,
                              file = NULL,
                              ...) {
  
  on.exit(print_messages())
  
  if (is.null(footnote)) 
    footnote <- paste0("MAD is the median average deviation with a ",
                       "consistency adjustment")
  

  .logical <- sapply(data, is.logical)
  if (any(.logical)) {
    for (i in seq_along(data)) {
      if (.logical[i]) data[[i]] <- as.numeric(data[[i]])
    }
    add_message(
      "Some variables were converted from logical to numeric: ",
      paste0(names(data)[.logical], collapse = ", ")
    )
  }
  
  .filter <- sapply(data, is.numeric)
  if (any(!.filter)) {
    add_message(
      "Some variables are not numeric and dropped from the analysis: ",
      paste0(names(.filter)[!.filter], collapse = ", ")
    )
  }
  
  data <- data[, .filter]
  
  if (auto_labels) data <- rename_from_labels(data)
  
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
  if (!is.null(round)) out <- round(out, round)
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
