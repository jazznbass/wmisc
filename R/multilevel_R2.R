#' Multilevel R squared for two-level models
#'
#' Calculates the multilevel R squared for two-level models.
#'
#' @param model1 The first model.
#' @param model2 The second model.
#' @param n Optional vector of group sizes for level 2.
#' @return A list with the following components: \item{R2.L1}{The R squared
#'   value for level 1.} \item{R2.L2}{The R squared value for level 2.}
#'   \item{R2.L1.2}{The combined R squared value for levels 1 and 2.}
#'   \item{R2.L2.2}{The adjusted R squared value for levels 1 and 2 (with
#'   optional group sizes).} \item{n}{The harmonic mean of the group sizes (if
#'   provided).}
#' @references Snijders, T. A. B., & Bosker, R. J. (1994). Modeled Variance in
#'   Two-Level Models. Sociological Methods & Research, 22(3), 342-363.
#'
#' @examples
#' data(sleepstudy, package = "lme4")
#' model1 <- lme(Reaction ~ 1, data = sleepstudy, random =~ 1|Subject)
#' model2 <- lme(Reaction ~ Days, data = sleepstudy, random =~ 1|Subject)
#' multilevel_R2(model1, model2, table(sleepstudy$Subject))
multilevel_R2 <- function(model1, model2, n = NULL) {
  v1 <- VarCorr(model1)
  v1.L1 <- as.numeric(v1[[2]][1])
  v1.L2 <- as.numeric(v1[[1]][1])
  v2 <- VarCorr(model2)
  v2.L1 <- as.numeric(v2[[2]][1])
  v2.L2 <- as.numeric(v2[[1]][1])
  R2.L1 <- 1 - (v2.L1 / v1.L1)
  R2.L2 <- 1 - (v2.L2 / v1.L2)
  R2.L1.2 <- 1 - ((v2.L1 + v2.L2) / (v1.L1 + v1.L2))
  R2.L2.2 <- NA
  n.harmonic <- NA
  if (!is.null(n)) {
    N <- length(as.numeric(n))
    n.harmonic <- (1 / N * sum(1 / n))^-1
    R2.L2.2 <- 1 - ((v2.L1 / n.harmonic + v2.L2) / (v1.L1 / n.harmonic + v1.L2))
  }
  cat("Multilevel R sqauered for two-level models \n")
  cat("Level 1:", R2.L1.2, "\n")
  cat("Level 2:", R2.L2.2, "\n")
  
  invisible(c(R2.L1 = R2.L1, R2.L2 = R2.L2, R2.L1.2 = R2.L1.2, R2.L2.2 = R2.L2.2, n = n.harmonic))
}

