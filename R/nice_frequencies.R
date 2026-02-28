#' Table with frequency statistics
#'
#' @param data Vector.
#' @param grouping A grouping variable as a vector.
#' @param label Set label for the variable name.
#' @param label_grouping Set label for the grouping variable name.
#' @param show_missing If TRUE, adds a row for the number of missing values.
#' @param show_percent If TRUE, adds a column for percentages.
#' @param percent_base If show_percent is TRUE, this argument specifies the base for percentage calculations.
#'  Options are "column" (percentages calculated within each column), "row"
#'  (percentages calculated within each row), or "total" (percentages calculated based on the total count).
#'  Default is "column".
#' @param auto_labels If TRUE, variable names are taken from a label attribute.
#' @param title Table title.
#' @param footnote Table footnote.
#' @param file Filename.
#' @param ... Further arguments passed to [nice_table()].
#' @return An html table with frequencies
#' @examples
#' nice_frequencies(wmisc:::mtcars_labeled[[11]])
#' nice_frequencies(
#'   wmisc:::mtcars_labeled$cyl, 
#'   wmisc:::mtcars_labeled$am
#' )
#' nice_frequencies(
#'  wmisc:::mtcars_labeled$cyl,
#'  wmisc:::mtcars_labeled$am,
#'  show_missing = FALSE,
#'  show_percent = FALSE
#' )
#' nice_frequencies(
#'   wmisc:::mtcars_labeled$cyl,
#'   wmisc:::mtcars_labeled$am,
#'   label = "Cylinders",
#'   label_grouping = "Transmission",
#'   title = "Cylinders by Transmission Type"
#' )
#' @export
nice_frequencies <- function(data,
                             grouping = NULL,
                             label = NULL,
                             label_grouping = NULL,
                             show_missing = TRUE,
                             show_percent = TRUE,
                             percent_base = "column",
                             add_total = TRUE, 
                             auto_labels = TRUE,
                             title = NULL,
                             footnote = NULL,
                             file = NULL,
                             round = 1,
                             ...) {
  
  init_messages(); on.exit(print_messages())
  
  if (show_percent && !percent_base %in% c("column", "row", "total", "cell")) {
    add_message(
      "Invalid value for 'percent_base'. Defaulting to 'column'."
    )
    percent_base <- "column"
  }
  
  useNA <- if (show_missing) "always" else "no"
  
  if (auto_labels && is.null(label)) 
    label <- get_label(data)
  if (auto_labels && is.null(label_grouping)) 
    label_grouping <- get_label(grouping)
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
    if (show_percent) {
      out$Percent = round(out[[1]]/sum(out[[1]], na.rm = TRUE)*100)
    }
    if (add_total) {
      .total <- sum(out[[1]], na.rm = TRUE)
      if (show_percent) {
        .total[2] <- sum(out$Percent, na.rm = TRUE)
      }
      out <- rbind(out, Total = .total)
    }
    
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
      if (percent_base == "column") {
        perc <- proportions(as.matrix(out) , margin = 2)
      } 
      if (percent_base == "row") {
        perc <- proportions(as.matrix(out) , margin = 1)        
      } 
      if (percent_base %in% c("total", "cell")) {
        perc <- proportions(as.matrix(out))
      } 
      perc <- as.data.frame(perc * 100)
      names(perc) <- paste0(" ", names(out), " ")
      if (add_total) {
        .total <- colSums(out, na.rm = TRUE)
        out <- rbind(out, Total = .total)
        if (percent_base == "column") {
          .total <- colSums(perc, na.rm = TRUE)
        } else {
          .total <- rep(NA, ncol(perc))
        } 
        perc <- rbind(perc, Total = .total)
        rn <- c(rn, "Total")
      }
      
      out <- cbind(out, perc)
    }
    
    if (!show_percent && add_total) {
      .total <- colSums(out, na.rm = TRUE)
      out <- rbind(out, Total = .total)
      rn <- c(rn, "Total")
    }
    
    spanner <- list(Frequency = 2:(group_levels + 1))
    names(spanner)[1] <- paste0(label_grouping, " (n)")
    if (show_percent) {
      spanner$Percent <- (group_levels + 2):(2 * group_levels + 1)
      names(spanner)[2] <- paste0(label_grouping, " (%)")
    }
  }
  
  if (length(which(is.na(rn))) > 0) rn[which(is.na(rn))] <- "Missing"
  if (length(which(rn == "NA.")) > 0) rn[which(rn == "NA.")] <- "Missing"

  rownames(out) <- NULL
  out <- cbind(Value = rn, out)
  names(out)[1] <- label
  
  if (show_percent && is.null(footnote) && !is.null(grouping)) {
    footnote <- paste0(
      "Percentages are calculated within ", 
      switch(percent_base, "column" = "columns", "row" = "rows", 
        "total" = "cells", "cell" = "cells")
    )
  } 
  
  out <- set_wmisc_attributes(out, 
    title = title,
    footnote = footnote,
    file = file,
    round = round,
    spanner = spanner,
    label_na = "-"
  )
  
  nice_table(out, ...)
}
