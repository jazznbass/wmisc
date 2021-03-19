#' Create a blogsite for Rmakrdown webpages
#'
#' Just a little helper for something like Blogs with Rmarkdown pages
#'
#' @param subdir Subdirectory where blog entries are saved
#' @param template A template file to create a blog-website from
#' @param file Output Rmd file with list of blog entries and references (based on template)
#' @param render Default is FALSE. When TRUE, all Markdown files in subdirectory are rendered to html. Otherwise, the function just takes available html files in the subdirectory folder
#' @param date_format A character string how to format the Date
#' @param filepattern File extensions that are included.
#' @param no_temp_files Logical. If TRUE, files (Markdown and html) starting with "_" are exluded
#' @param root Root directory of the project.
#'
#' @return A markdown file in the project directory.
#' @export

create_blogsite <- function(subdir = "posts",
                            template = "_blog.Rmd",
                            file = "blog.Rmd",
                            render = FALSE,
                            date_format = "%A, %B %d",
                            filepattern = c(".rmd", ".md", ".Rmd"),
                            no_temp_files = TRUE, # files starting with "_" are not considered
                            root = getwd()) {
  
  on.exit(setwd(root))
  setwd(root)
  index <- readLines(template)
  
  setwd(file.path(root, subdir))
  files <- dir()
  files <- files[which(str_sub(files, -4) %in% filepattern)]
  if (no_temp_files) files <- files[!startsWith(files, "_")]
  
  # extracts dates and titles -----------------------------------------------
  title <- list()
  date <- list()
  abstract <- list()
  
  
  for(i in seq_along(files)) {
    lines <- readLines(files[i], n = 10)
    
    tmp <- startsWith(lines, "title:")
    if (any(tmp)) {
      title[i] <- lines[tmp]
    } else {
      title[i] <- paste0("title: ", str_sub(files[i], end = -4), collapse = "") 
    }
    
    title[i] <- trimws(str_sub(title[i], 7))   
    
    tmp <- startsWith(lines, "date:")
    if (any(tmp)) {
      date[i] <- lines[tmp]
    } else {
      date[i] <- "date: 1972-04-29"
    }
    date[i] <- trimws(str_sub(date[i], 6))
    
    tmp <- startsWith(lines, "abstract:")
    if (any(tmp)) {
      abstract[i] <- lines[tmp]
    } else {
      abstract[i] <- ""
    }
    abstract[i] <- trimws(str_sub(abstract[i], 10))
  }  
  
  date <- lapply(date, as.Date)
  
  # render all files in posts folder ----------------------------------------
  
  if (render) for(file in files) rmarkdown::render(file)
  
  # appends _blog.Rmd file with links -------------------------------------
  
  posts <- dir(file.path(root, subdir))
  posts <- posts[which(endsWith(posts, ".html"))]
  if (no_temp_files) posts <- posts[!startsWith(posts, "_")]
  
  add <- c()
  year <- "2000"
  for(i in order(unlist(date), decreasing = TRUE)) {
    
    if (format(date[[i]], format = "%Y") != year) {
      year <- format(date[[i]], format = "%Y")
      add <- c(add, paste0("## ", year, collapse = ""), "")
    }
    
    add <- c(add, paste0("### ", format(date[[i]], format = date_format), collapse = ""))
    
    link <- paste0('./', subdir, '/',posts[i], collapse = "")
    text <- paste0("**", title[[i]] ,"**", collapse = "")
    link <- paste0("### <a href = '", link, "'>", text, "</a>", collapse = "")
    add <- c(add, "", link, "")
    if (nchar(abstract[[i]]) > 0) {
      abst <- paste0("", abstract[[i]], collapse = "")
      add <- c(add, abst, "")
    }
  }
  
  new_index <- c(index, add)
  
  setwd(root)
  
  writeLines(new_index, file)
}
