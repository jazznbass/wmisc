add_row <- function(base, addon) {
  base[names(addon)[!(names(addon) %in% names(base))]] <- NA
  addon[names(base)[!(names(base) %in% names(addon))]] <- NA
  rbind(base, addon)
}

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
