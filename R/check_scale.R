#' Check scale for typos
#'
#' @param data A data-frame
#' @param var.names Character vector with variable names
#' @param min minimum of scale
#' @param max maximum of scale
#'
#' @return A dataframe containing all cases with valies above and below the alowed values.
#' @export

check_scale <- function(data, var.names, min, max) {
  N <- length(var.names)
  min <- rep_len(min, N)
  max <- rep_len(max, N)

  for (i in 1:N) {
    error <- NA
    error <- which(data[, var.names[i]] < min[i] | data[, var.names[i]] > max[i])
    df.error <- data.frame(case = error, value = data[, var.names[i]])
    if (!is.na(error[1])) {
      cat(var.names[i], ":\n")
      cat(error, sep = ",")
      cat("\n")
      print(df.error)
    }
  }
}
