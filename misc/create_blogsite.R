#' Create a blogsite for Rmakrdown webpages
#'
#' Just a little helper for something like Blogs with Rmarkdown pages
#'
#' @param source_dir A directory path with blogposts 
#' @param template A template file to create a blog-website from
#' @param outfile Output Rmd file with list of blog entries and references (based on template)
#' @param subdir Subdirectory where blog entries will be saved
#' @param render Default is FALSE. When TRUE, all Markdown files in subdirectory are rendered to html. Otherwise, the function just takes available html files in the subdirectory folder
#' @param date_format A character string how to format the Date
#' @param filepattern File extensions that are included.
#' @param no_temp_files Logical. If TRUE, files (Markdown and html) starting with "_" are exluded
#' @param root Root directory of the project.
#'
#' @return A markdown file in the project directory.
#' @export


create_blogsite <- function(source_dir = "posts",
                            template = "_blog.Rmd",
                            outfile = "blog.Rmd",
                            subdir = "posts",
                            render = FALSE,
                            date_format = "%A, %B %d",
                            filepattern = c(".rmd", ".md", ".Rmd"),
                            no_temp_files = TRUE, # files starting with "_" are not considered
                            root = getwd()) {
  
  
  on.exit(setwd(root))
  
  args <-  as.list(environment()) # c(as.list(environment()), list(...))
  
  if (!any(endsWith(dir(), ".Rproj"))) stop("No project directory found!")
  if (!file.exists(("_site.yml"))){ 
    warnings("_site.yml not found!")
  } else {
    yaml <- yaml::read_yaml("_site.yml")$blog
    args <- c(yaml, args)
    args <- args[unique(names(args))]
  }

  do.call(.create_blogsite, args)
}

.create_blogsite <- function(...) {
  
  args <- list(...)
  setwd(args$root)
  index <- readLines(args$template, encoding = "UTF-8")
  
  # copy files with posts to local directory --------------------------------
  
  if (!identical(args$source_dir, args$subdir)) {
    unlink(args$subdir, recursive = TRUE)  
    dir.create(args$subdir, showWarnings = FALSE)
    setwd(args$source_dir)
    source <- dir()
    source <- source[!endsWith(source, ".Rproj")]
    source <- source[source != "README.md"]
    file.copy(source, file.path(args$root, args$subdir), recursive = TRUE)
  }
  
  # extracts dates and titles -----------------------------------------------
  
  setwd(file.path(args$root, args$subdir))
  files <- dir()
  files <- files[which(substr(files, nchar(files) - 3, nchar(files)) %in% args$filepattern)]
  if (args$no_temp_files) files <- files[!startsWith(files, "_")]
  
  title <- list()
  date <- list()
  abstract <- list()
  
  for(i in seq_along(files)) {
    
    yaml <- rmarkdown::yaml_front_matter(files[i])
    
    if (!is.null(yaml$title)) {
      title[i] <- yaml$title
    } else {
      title[i] <- paste0(substr(files[i], 1, nchar(files[i]) - 4 ), collapse = "") 
    }
    
    if (!is.null(yaml$date)) {
      date[i] <- yaml$date
    } else {
      date[i] <- "1972-04-29"
    }
    
    if (!is.null(yaml$abstract)) {
      abstract[i] <- yaml$abstract
    } else {
      abstract[i] <- ""
    }
  }  
  
  date <- lapply(date, as.Date)
  
  # render all files in posts folder ----------------------------------------
  
  if (args$render) for(file in files) rmarkdown::render(file, encoding = "UTF-8")
  
  # appends _blog.Rmd file with links -------------------------------------
  
  posts <- dir(file.path(args$root, args$subdir))
  posts <- posts[which(endsWith(posts, ".html"))]
  if (args$no_temp_files) posts <- posts[!startsWith(posts, "_")]
  
  add <- c()
  year <- "2000"
  
  for(i in order(unlist(date), decreasing = TRUE)) {
    
    # Year
    if (format(date[[i]], format = "%Y") != year) {
      year <- format(date[[i]], format = "%Y")
      add <- c(
        add, 
        paste0("<h1 class = 'blog_year'>", year, "</h1>", collapse = ""), 
        ""
      )
    }
    
    # Date
    add <- c(
      add, 
      paste0("<p class = 'blog_date'>", format(date[[i]], format = args$date_format), 
             "</p>",collapse = ""
      )
    )
    
    # Title
    link <- paste0('./', args$subdir, '/', posts[i], collapse = "")
    text <- paste0("<p class = 'blog_title'>", title[[i]] ,"</p>", collapse = "")
    
    add <- c(add, 
             "", 
             paste0(
               "<a href = '", link, "' class = 'blog_link'>", text, "</a>", 
               collapse = ""
             ), 
             ""
    )
    
    # Abstract
    if (nchar(abstract[[i]]) > 0) {
      abst <- paste0(
        "<p class = 'blog_abstract'>", abstract[[i]], "</p>", collapse = ""
      )
      add <- c(add, abst, "")
    }
  }
  
  new_index <- c(index, add)
  
  setwd(args$root)
  
  out <- file(args$outfile, encoding = "UTF-8")
  writeLines(new_index, out)
  close(out)
  
  #Sys.setlocale("LC_TIME", lct)
  
}

