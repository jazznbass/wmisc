#' Fills in missing data for wide format
#' 
#' 
#' @param data A data frame
#' @param id Character string with L2 variable
#' @param vars Vector of strings with varible names
#'
#' @return A data frame with replaced values
#' @export
#'
#' @examples
#' x <- data.frame(id = rep(1:3, each = 3), gender = c(1, 1, NA, 0, 0, 0, 1, NA, NA))
#' fill_missing_l2(x, "id", "gender")
#' 
#' x <- data.frame(id = rep(1:3, each = 3), gender = c(1, 1, NA, 0, 0, 0, 1, 0, NA))
#' fill_missing_l2(x, "id", "gender")
fill_missing_l2 <- function(data, id, vars) {
  
  units <- unique(data[[id]])
  
  out_string <- "Replaced at id "
  
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
