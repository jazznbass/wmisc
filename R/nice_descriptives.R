#' Table with descriptive statistics
#'
#' @param data A data frame
#' @param round Digits for round function
#' @param labels Deprecated. Use [rename_items()] instead.
#'
#' @return A data frame with descriptive statistics
#' @examples
#' nice_descriptives(mtcars)
#' @export
nice_descriptives <- function(data, round = 2, labels = FALSE) {
  
  msg <- c()
  
  .filter <- sapply(data, is.numeric)
  
  if (any(!.filter)) {
    msg <- c(msg, paste0(
      "Some variables are not numeric and dropped from the analysis: ",
      paste0(names(.filter)[!.filter], collapse = ", ")
    ))
  }
  
  data <- data[, .filter]
  
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
  out <- round(out, round)
  out <- data.frame(out)
  out <- cbind(Variable = rownames(out), out)
  rownames(out) <- NULL
  
  return_messages(msg)
  
  out
}

