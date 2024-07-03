#' Table with frequency statistics
#'
#' @param data Vector.
#' @param grouping A grouping variable as a vector.
#' @param label Set label for the variable name.
#' @param show_missing If TRUE, adds a row for the number of missing values.
#' @param show_percent If TRUE, adds a column for percentages.
#' @param auto_labels If TRUE, variable names are taken from a label attribute.
#' @param title Table title.
#' @param footnote Table footnote.
#' @param file Filename.
#' @param ... Further arguments passed to [nice_table()].
#' @return An html table with frequencies
#' @examples
#' nice_frequencies(mtcars[[11]])
#' nice_frequencies(
#'   mtcars$cyl, mtcars$am, 
#'   label = "Cylinders", 
#'   label_grouping = "Automatic"
#' )
#' @export
nice_frequencies <- function(data,
                             grouping = NULL,
                             label = NULL,
                             label_grouping = NULL,
                             show_missing = TRUE,
                             show_percent = TRUE,
                             auto_labels = TRUE,
                             title = NULL,
                             footnote = NULL,
                             file = NULL,
                             ...) {
  
  on.exit(print_messages())
  
  useNA <- if (show_missing) "always" else "no"
  
  if (auto_labels && is.null(label)) 
    label <- attr(data, "label")
  if (auto_labels && is.null(label_grouping)) 
    label_grouping <- attr(grouping, "label")
  if (is.null(label)) 
    label <- deparse(substitute(data))
  if (is.null(label_grouping)) 
    label_grouping <- deparse(substitute(grouping))
  if (is.null(title) && is.null(grouping)) {
    title <- paste0("Frequency statistics of '", label, "'")
  }
  
  if (is.null(title) && !is.null(grouping)) {
    title <- paste0(
      "Frequency statistics of '", label, "' by '", label_grouping, "'"
    )
  }
  
  
  if (is.null(grouping)) {
    tab <- table(data, useNA = useNA)
    class(tab) <- NULL
    out <- as.data.frame(tab)
    names(out)[1] <- "Frequency"
    if (show_percent)
      out$Percent = round(out[[1]]/sum(out[[1]], na.rm = TRUE)*100)
    spanner <- NULL
    rn <- rownames(out)
  } else {
    tab <- table(data, grouping, useNA = useNA)
    class(tab) <- NULL
    out <- as.data.frame(tab)
    out[ncol(out)] <- NULL
    rn <- row.names(tab)
    group_levels <- ncol(out)
  
    if (show_percent) {
      perc <- lapply(out, \(.) round(./sum(., na.rm = TRUE)*100))
      perc <- as.data.frame(perc)
      names(perc) <- paste0(" ", names(out), " ")
      
      out <- cbind(out, perc)
      
    }
    spanner <- list(
      Frequency = 2:(group_levels + 1), 
      Percent = (group_levels + 2):(2 * group_levels + 1)
    )
    names(spanner)[1] <- label_grouping#paste0("Frequency '", label_grouping, "'")
  }
  
  if (length(which(is.na(rn))) > 0) rn[which(is.na(rn))] <- "Missing"
  if (length(which(rn == "NA.")) > 0) rn[which(rn == "NA.")] <- "Missing"

  
  rownames(out) <- NULL
  out <- cbind(Value = rn, out)
  names(out)[1] <- label
  
  out <- set_wmisc_attributes(out, 
    title = title,
    footnote = footnote,
    file = file,
    spanner = spanner
  )
  
  nice_table(out, ...)
}
