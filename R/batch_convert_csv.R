#' Batch convert csv
#' Convert all csv fies in a folder to xlsx and rds files
#'
#' @param folder Character string with source folder
#' @param xlsx If TRUE creates xlsx files.
#' @param rds If TRUE creates rds files.
#' @param move_csv If not an empty string it names a folder to move csv to after conversion
#' @param dec Decimal sign
#' @param sep Separator sign
#' @param ... Further functions for the underlying read.csv. (e.g. fileEncoding = "WINDOWS-1252")
#'
#' @return Writes xlsx and/or rds files into the source folder for each csv file.
#' @export

batch_convert_csv <- function(folder, xlsx = TRUE, rds = TRUE, move_csv = "", dec = ",", sep = ";", ...) {
  
  if (missing(folder)) folder <- getwd()
  dir_working <- getwd()
  setwd(folder)
  filenames <- dir(patter = "*.csv")
  
  if (move_csv != "")  dir.create(move_csv)
  
  for(name in filenames) {
    infile <- read.csv(
      name, dec = dec, sep = sep, stringsAsFactors = FALSE, strip.white = TRUE, 
      check.names = FALSE, ...
    )
    if (xlsx) write.xlsx(infile, paste0(substr(name, 1, nchar(name) - 3), "xlsx")) 
    if (rds) saveRDS(infile, paste0(substr(name, 1, nchar(name) - 3), "rds")) 
    
    if (move_csv != "") {
      file.copy(name, move_csv)
      unlink(name)
    }
  }
  
  setwd(dir_working)
  
}

