#' Table with frequency statistics
#'
#' @param data A data frame
#' @param label Set label for the variable name.
#' @param show_missing If TRUE, adds a row for the number of missing values.
#' @param auto_labels If TRUE, variable names are taken from a label attribute.
#' @param title Table title.
#' @param footnote Table footnote.
#' @param file Filename.
#' @param ... Further arguments passed to [nice_table()].
#' @return An html table with frequencies
#' @examples
#' nice_frequencies(mtcars[[11]])
#' @export
nice_frequencies <- function(data,
                             label = NULL,
                             show_missing = TRUE,
                             auto_labels = TRUE,
                             title = NULL,
                             footnote = NULL,
                             file = NULL,
                             ...) {
  
  on.exit(print_messages())
  if (auto_labels && is.null(label)) label <- attr(data, "label") 
  if (is.null(label)) label <- deparse(substitute(data))
  if (is.null(title)) {
    title <- paste0("Frequency statistics of '", label, "'")
  }
  
  #if (is.null(footnote)) 
  #  footnote <- paste0("MAD is the median average deviation with a ",
  #                     "consistency adjustment")
  
  
  #.filter <- sapply(data, is.numeric)
  
  #if (any(!.filter)) {
  #  add_message(
  #    "Some variables are not numeric and dropped from the analysis: ",
  #    paste0(names(.filter)[!.filter], collapse = ", ")
  #  )
  #}
  
  #data <- data[, .filter]
  
  #if (auto_labels) data <- rename_from_labels(data) 
  
  tab <- if (show_missing) table(data, useNA = "always") else table(data)
  class(tab) <- NULL
  out <- as.data.frame(tab)
  names(out)[1] <- "Frequency"
  
  rn <- rownames(out)
  if (length(which(is.na(rn))) > 0) rn[which(is.na(rn))] <- "Missing"
  
  rownames(out) <- NULL
  out <- cbind(Value = rn, out)
  
  out$Percent = round(out[[2]]/sum(out[[2]], na.rm = TRUE)*100)
  
  out <- set_wmisc_attributes(out, 
    title = title,
    footnote = footnote,
    file = file
  )
  
  nice_table(out, ...)
}
