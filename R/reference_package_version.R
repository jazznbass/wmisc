#' Reference package version for markdown
#'
#' @param packages Vector of strings with package names.
#' @param ... Package names
#'
#' @return A markdown formatted for citation of the packages.
#' @export

reference_package_version <- function(packages, ...) {
  
  packages2 <- list(...)
  packages <- as.list(packages)
  packages <- c(packages, packages2)
  out <- lapply(packages, function(x)   
    paste0("Package `", x, "` at version ", packageVersion(x), " [@R-", x, "].  ")
  )
  cat(paste0(unlist(out), collapse = "\n"))
}
