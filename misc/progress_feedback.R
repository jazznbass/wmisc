#' @export
sec_time <- function(sec) {
  sec <- round(sec)
  h <- sec %/% 3600
  minutes <- (sec - h * 3600) %/% 60
  sec <- sec - (minutes * 60 + h * 3600)
  sprintf("%d:%02d:%02d", h, minutes, sec)
}

#' @export
progress_feedback <- function(counter, total, seconds) {
  if (counter == 0) {
    perc_done <- 0
    sec_remain <- NA
  } else {
    perc_done <- counter / total * 100
    sec_remain <- (100 - perc_done) * (seconds / perc_done)
  }
  cat(
    "\rProgress ",
    sprintf("%3d", round(perc_done)), "%; ",
    sec_time(seconds), "; remain ",
    sec_time(sec_remain),
    sep = ""
  )
}
