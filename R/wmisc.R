##### wilberts misc R functions####

.onAttach <- function(lib, pkg, ...) {
  out <- paste0(
    "wmisc ", utils::packageVersion("wmisc"),
    " (", utils::packageDate('wmisc'), ")\n"
  )
  packageStartupMessage(out)
}

.onLoad <- function(lib, pkg, ...) {

}
