#' Update this package from GitHub if a newer version is available
#' @export
update_self <- function() {
  pkg <- "wmisc"
  repo <- "jazznbass/wmisc"
  current <- utils::packageVersion(pkg)
  deps <- remotes::package_deps(repo, dependencies = FALSE)
  i <- which(deps$package %in% pkg)
  remote <- if (length(i)) deps$available[i] else NA_character_
  
  if (!is.na(remote) &&
      utils::compareVersion(as.character(remote), as.character(current)) > 0) {
    message("Updating ", pkg, " from ", current, " to ", remote)
    remotes::install_github(repo, upgrade = "never", quiet = TRUE)
  } else {
    message(pkg, " is up to date (", current, ").")
  }
}
