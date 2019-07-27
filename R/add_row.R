add_row <- function(base, addon) {
  base[names(addon)[!(names(addon) %in% names(base))]] <- NA
  addon[names(base)[!(names(base) %in% names(addon))]] <- NA
  rbind(base, addon)
}
