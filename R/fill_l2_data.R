#' Fill Missing Level-2 Variables in Long Format Data
#'
#' In long-format datasets, level-2 (L2) variables may be partially missing
#' within groups (e.g., subject ID). This function fills in missing L2 values
#' using available information from other rows in the same group.
#'
#' @param data A data frame in long format.
#' @param id Character string. Name of the grouping (L2) variable, e.g., "id".
#' @param vars Character vector of variable names (L2 variables) whose missing
#'   values should be filled within groups.
#'
#' @return A data frame with filled-in L2 values. If conflicting non-missing
#'   values exist within a group, a message is printed and values are not imputed.
#'
#' @examples
#' x <- data.frame(
#'   id = rep(1:5, each = 3),
#'   gender = c(1, 1, NA, 0, 0, 0, 1, NA, NA, NA, NA, NA, 1, 0, NA)
#' )
#' fill_missing_l2(x, "id", "gender")
#'
#' @export
fill_missing_l2 <- function(data, id, vars) {
  
  units <- unique(data[[id]])
  
  out_string <- "Complemented at id: "
  
  for(column in vars) {
    
    for(unit in units) {
      
      cases <- which(data[[id]] == unit)
      nas <- which(data[[id]] == unit & is.na(data[[column]]))
      not_nas <- which(data[[id]] == unit & !is.na(data[[column]]))
      values <- unique(data[[column]][not_nas])
      if (length(values) > 1) {
        cat(
          "Conflicting values in id", unit, ", cases ", 
          paste0(not_nas, collapse = ", "), "\n"
        )
        next
      }
      if (length(values) == 1 && length(nas) > 0) {
        data[[column]][nas] <- values 
        out_string <- c(out_string, as.character(unit))
      } 
    }
    if (length(out_string) > 1) cat(paste(out_string, collapse = " "), "\n")
  }
  
  data
}
