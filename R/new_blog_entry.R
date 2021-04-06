#' Create blog entry
#'
#' @param title Title
#' @param author Author
#' @param date Date
#' @param abstract Abstract
#' @param keywords Keywords
#' @param subdir Directory with posts
#' @param ... Further YAML parameters
#'
#' @return Nothing
#' @export
new_blog_entry <- function(title = "New Blog Entry",
                       author = "Juergen Wilbert", 
                       date = format(Sys.time(), "%Y-%m-%d"), 
                       abstract = "", 
                       keywords = "blog", 
                       subdir = "posts",
                       ...) {
  
  if (!any(endsWith(dir(), ".Rproj"))) stop("No project directory found!")
  out <- c(as.list(environment()), list(...))
  filename <- file.path(subdir, paste0(out$date, ".Rmd", collapse = ""))
  i <- 1
  while(file.exists(filename)) {
    i <- i + 1
    filename <- file.path(subdir, paste0(out$date, "_", i, ".Rmd", collapse = ""))
  }
  fileout <- file(filename, "w", encoding = "UTF-8")  
  cat("---\n", file = fileout)
  yaml::write_yaml(out, fileout)
  cat("---\n", file = fileout)
  close(fileout)
  file.edit(filename)
}

