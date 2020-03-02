##### wilberts misc R functions####

.onAttach <- function(lib, pkg, ...) {
  out <- paste0(
    "Wmisc ", utils::packageVersion("Wmisc"),
    " (", utils::packageDate('Wmisc'), ")\n"
  )
  packageStartupMessage(out)
}

.onLoad <- function(lib, pkg, ...) {

}
