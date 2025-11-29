#' Create a description of a dataset
#'
#' Generates a concise textual description of a dataset, summarizing each column's type, 
#' number of missing values, and a short overview of its content (e.g., range or levels). 
#' Optionally, this description can be written to a `README.md` file.
#' Note: this helpfile was generated with AI
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
create_data_description <- function(
    dat, readme = FALSE, tab = " | ", max_char = 60, overwrite = TRUE) {
  
  filename <- as.character(match.call()[2])
  file_info <- NULL
  if (is.character(dat)) {
    filename <- dat
    ext <- tools::file_ext(filename)
    if (ext %in% c("rds", "RDS")) dat <- readRDS(filename)
    file_info <- file.info(filename)
    
    if (!inherits(dat, "data.frame")) {
      warning(filename, "is not a data frame")
      return(FALSE)
    }
  }
 

  info <- sapply(dat, \(.) {
    out <- "-"
    if (is.character(.)) . <- as.factor(.)
    if (is.factor(.)) out <- paste0(levels(.), collapse = ", ")
    if (is.numeric(.)) {
      tryCatch(
        out <- paste0(round(min(., na.rm = TRUE), 2), " to ", round(max(., na.rm = TRUE), 2)), 
        error = function(e) NULL
      )
    }
      
    out <- iconv(out, from = "", to = "UTF-8", sub = "byte")
    if (nchar(out) > max_char) out <- paste0(substr(out, 1, max_char), " [...]")
    
    out
  })
  
  names <- names(dat) |> iconv(from = "", to = "UTF-8", sub = "byte")
  
  cl <- sapply(dat, \(.) paste0(class(.), collapse = "/"))
  cl <- paste0("(", cl, ", ", sapply(dat, \(.) sum(is.na(.))), " NA) ", sep = "")
  
  #max_chars_cl <- max(nchar(cl))
  #cl <- sprintf(paste0("%-", max_chars_cl, "s"), cl)
  
  #max_chars_info <- max(nchar(info))
  
  ## ---
  
  out <- data.frame(Variable = names, Class = cl, Info = info)
  
  if (readme) {
    fn <- file.path(dirname(filename), paste0("README-", basename(filename), ".md"))
    if (!overwrite) if (file.exists(fn)) return(FALSE)
    sink(fn, append = FALSE)
  }
  
  cat("# Discription of datafile `", basename(filename), "`", sep = "")
  cat("\n\n")
  if (!is.null(file_info)) cat("Datafile from:", as.character(file_info$mtime), "  \n")
  cat("Columns: ", ncol(dat), " / Rows: ", nrow(dat), sep = "")
  cat("  \n\n")
  
  cat(knitr::kable(out, format = "markdown", row.names = FALSE), sep = " \n")
  
  if (readme) sink()
  
}

#' @export
#' @param recursive If TRUE includes subdirectories
#' @describeIn create_data_description Automatically create data description files for all .rds files in a directory
batch_create_data_description <- function(recursive = FALSE, overwrite = FALSE) {
  x <- sapply(
    list.files(pattern = "\\.rds$", recursive = recursive, full.names = TRUE), 
    function(x) {
      cat(x, "\n")
      create_data_description(x, readme = TRUE, overwrite = overwrite)
    }
  )
 
  cat("Wrote", sapply(x, function(x) !isFALSE(x)) |> sum(), "files\n")
  cat("Skipped", sapply(x, isFALSE) |> sum(), "files\n")
  
}

