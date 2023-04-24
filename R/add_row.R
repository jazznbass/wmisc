add_row <- function(base, addon) {
  base[names(addon)[!(names(addon) %in% names(base))]] <- NA
  addon[names(base)[!(names(base) %in% names(addon))]] <- NA
  rbind(base, addon)
}

#' Combine multiple data frames into a single data frame
#' 
#' This function takes in multiple data frames and combines them into a single data frame. Missing columns and rows between data frames are filled with `NA` values. 
#'
#' @param ... The data frames to combine
#'
#' @return A combined data frame
#' 
#' @export
#' 
#' @examples
#' a <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
#' b <- data.frame(id = 4:6, age = c(25, 30, 35))
#' combine_data_frames(a,b)
#' # id    name age
#' # 1  Alice  NA
#' # 2    Bob  NA
#' # 3 Charlie  NA
#' # 4     NA  25
#' # 5     NA  30
#' # 6     NA  35
#' @export
combine_data_frames <- function(...) {
  dfs <- list(...)
  
  add <- function(base, addon) {
    base[names(addon)[!(names(addon) %in% names(base))]] <- NA
    addon[names(base)[!(names(base) %in% names(addon))]] <- NA
    rbind(base, addon)
  }
  
  out <- dfs[[1]]
  for(i in 2:(length(dfs) - 1)) {
    out <- add(out, dfs[[i]])
  }
  out
}
