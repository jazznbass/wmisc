#' Analyse missing values
#'
#' @param dat A data-frame
#' @param scale A list with character vectors. Each list element containing the variable names of a scale
#'
#' @return A data-frame
#' @export

analysis_missing <- function(dat, scale) {
  N <- length(scale)
  cases <- nrow(dat)
  p.total <- data.frame("missing" = NA, "total" = NA, "p" = NA)
  for (i in 1:N) {
    n <- length(scale[[i]]) * nrow(dat)
    # dat[,scale[[i]]]

    p.total[i, "missing"] <- sum(is.na(dat[, scale[[i]]]))
    p.total[i, "total"] <- length(scale[[i]]) * nrow(dat)
    p.total[i, "p"] <- sum(is.na(dat[, scale[[i]]])) / (length(scale[[i]]) * nrow(dat))
    p.total[i, "n.cases"] <- sum(unlist(apply(dat[, scale[[i]]], 1, function(x) any(is.na(x)))))
    p.total[i, "p.cases"] <- sum(unlist(apply(dat[, scale[[i]]], 1, function(x) any(is.na(x))))) / cases
    p.total[i, "n.all.cases"] <- sum(unlist(apply(dat[, scale[[i]]], 1, function(x) all(is.na(x)))))
    p.total[i, "p.all.cases"] <- sum(unlist(apply(dat[, scale[[i]]], 1, function(x) all(is.na(x))))) / cases
  }
  p.total
}
