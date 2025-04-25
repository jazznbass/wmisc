#' Create a description of a dataset
#'
#' Generates a concise textual description of a dataset, summarizing each column's type, 
#' number of missing values, and a short overview of its content (e.g., range or levels). 
#' Optionally, this description can be written to a `README.md` file.
#' [this helpfile was generated with AI]
#'
#' @param dat A data.frame or a character string pointing to an `.rds` file to load. 
#'   If a character string is provided and ends with `.rds`, the file will be loaded using `readRDS()`.
#' @param readme Logical. If `TRUE`, the output is appended to a file named `"README.md"`. Default is `FALSE`.
#' @param tabs Integer. Number of tab characters (`\\t`) to use for formatting. Default is `1`.
#' @param max_char Integer. Maximum number of characters to show per variable summary. 
#'   Longer content will be truncated. Default is `60`.
#'
#' @return Invisibly returns a character vector with one entry per column describing its contents. 
#'   Side effect: prints the description to the console, and optionally to a README file.
#'
#' @details
#' For each column in the dataset, the function provides:
#' - The column name
#' - The type (`typeof`)
#' - The number of missing values
#' - A brief summary:
#'   - For factors: list of levels (possibly truncated)
#'   - For numeric variables: value range
#'   - For character variables: coerced to factor and listed as above
#'
#' @examples
#' df <- data.frame(
#'   id = 1:10,
#'   group = factor(c("A", "B")),
#'   score = c(NA, 2:10)
#' )
#' create_data_description(df)
#'
#' @export
create_data_description <- function(dat, readme = FALSE, tab = "   ", max_char = 60) {
  
  filename <- as.character(match.call()[2])
  
  if (is.character(dat)) {
    filename <- dat
    ext <- tools::file_ext(filename)
    if (ext == "rds") dat <- readRDS(dat)
    
  }
  
  info <- sapply(dat, \(.) {
    out <- NA
    if (is.character(.)) . <- as.factor(.)
    if (is.factor(.))
      out <- paste0(levels(.), collapse = ", ")
    if (is.numeric(.))
      out <- paste0(round(min(., na.rm = TRUE), 2), " to ", round(max(., na.rm = TRUE), 2))
    if (nchar(out) > max_char) out <- paste0(substr(out, 1, max_char - 6), " [...]")
    out
  })
  
  names <- names(dat)
  names <- sprintf(paste0("%-", max(nchar(names)), "s"), names)
  
  cl <- sapply(dat, \(.) paste0(class(.), collapse = "|"))
  cl <- paste0("(", cl, ", ", sapply(dat, \(.) sum(is.na(.))), " NA):", sep = "")
  cl <- sprintf(paste0("%-", max(nchar(cl)), "s"), cl)
  
  
  out <- paste0(names, tab, cl, tab, info, sep = "")
  
  if (readme) {
    fn <- paste0("README-", filename, ".md")
    sink(fn, append = FALSE)
  }
  
  cat("# Discription of datafile `", filename, "`", sep = "")
  cat("\n\n")
  cat("Columns: ", ncol(dat), " | Rows: ", nrow(dat), sep = "")
  cat("\n\n")
  cat(out, sep = "\n")
  
  if (readme) sink()
  
}
