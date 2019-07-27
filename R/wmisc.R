##### wilberts misc R functions####

.onAttach <- function(lib, pkg, ...) {
  out <- paste0(
    "Wmisc - Wilbert miscellaneous functions\n",
    "Caution! This is a beta version and heavily under construction!\n"
  )
  packageStartupMessage(out)
}

.onLoad <- function(lib, pkg, ...) {

}
