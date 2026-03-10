## code to prepare `DATASET` dataset goes here

library(usethis)

e <- new.env()
load("data-raw/sysdata.rda", envir = e)

list2env(as.list(e, all.names = TRUE), envir = .GlobalEnv)

attr(mtcars_labeled$am, "labels") <- c("Automatic" = 0, "Manual" = 1)
attr(mtcars_labeled$am, "label") <- "Transmission Type"

attr(mtcars_labeled$vs, "labels") <- c("V-shaped" = 0, "Straight" = 1)
attr(mtcars_labeled$vs, "label") <- "Engine Type"

filenames <- ls(e)
for(i in seq_along(filenames))
  do.call("use_data", list(as.name(filenames[i]), overwrite = TRUE))

rm(list = filenames)
rm(list = c("e", "filenames"))
