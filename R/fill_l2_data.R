#' Fills in missing level 2 data
#' 
#' For cases where you have a long format data set and it contains level 2 data but some of the level 2 data are missing although the necessary information in available in other data rows of the same grouping value.
#' 
#' @param data A data frame
#' @param id Character string with L2 variable
#' @param vars Vector of strings with variable names
#'
#' @return A data frame with added values. It also reports if l2 values conflict.
#' @export
#'
#' @examples
#' x <- data.frame(
#'   id = rep(1:5, each = 3), 
#'   gender = c(1, 1, NA, 0, 0, 0, 1, NA, NA, NA, NA, NA, 1, 0, NA)
#' )
#' x
#' fill_missing_l2(x, "id", "gender")
fill_missing_l2 <- function(data, id, vars) {
  
  units <- unique(data[[id]])
  
  out_string <- "Complemented at id: "
  
  for(i in seq_along(vars)) {
    
    for(k in seq_along(units)) {
      
      cases <- which(data[[id]] == units[k])
      nas <- which(data[[id]] == units[k] & is.na(data[[vars[i]]]))
      not_nas <- which(data[[id]] == units[k] & !is.na(data[[vars[i]]]))
      values <- unique(data[[vars[i]]][not_nas])
      if (length(values) > 1) {
        
        cat(
          "Conflicting values in id", units[k], ", cases ", 
          paste0(not_nas, collapse = ", "), "\n"
        )
      }
      if (length(values) == 1 && length(nas) > 0) {
        
        data[[vars[i]]][nas] <- values 
        #data[nas, vars[i]] <- values
        out_string <- c(out_string, as.character(units[k]))
      } 
      
    }
    
    if (length(out_string) > 1) cat(paste(out_string, collapse = " "), "\n")
  }
  
  data
}
