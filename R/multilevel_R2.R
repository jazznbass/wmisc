#' Multilevel R squared for two-level models
#'
#' Calculates the multilevel R squared for two-level models.
#'
#' @param model1 The first model.
#' @param model2 The second model.
#' @param n Optional vector of group sizes for level 2.
#' @return A list with the following components: \item{r2_l1}{The R squared
#'   value for level 1.} \item{r2_l2}{The R squared value for level 2.}
#'   \item{r2_l1_2}{The combined R squared value for levels 1 and 2.}
#'   \item{r2_l2_2}{The adjusted R squared value for levels 1 and 2 (with
#'   optional group sizes).} \item{n}{The harmonic mean of the group sizes (if
#'   provided).}
#' @references Snijders, T. A. B., & Bosker, R. J. (1994). Modeled Variance in
#'   Two-Level Models. Sociological Methods & Research, 22(3), 342-363.
#'
#' @examples
#' data(sleepstudy, package = "lme4")
#' model1 <- nlme::lme(Reaction ~ 1, data = sleepstudy, random =~ 1|Subject)
#' model2 <- nlme::lme(Reaction ~ Days, data = sleepstudy, random =~ 1|Subject)
#' multilevel_r2(model1, model2, table(sleepstudy$Subject))
#' @export
multilevel_r2 <- function(model1, model2, n = table(model1$groups)) {
  v1 <- VarCorr(model1)
  v1_l1 <- as.numeric(v1[[2]][1])
  v1_l2 <- as.numeric(v1[[1]][1])
  v2 <- VarCorr(model2)
  v2_l1 <- as.numeric(v2[[2]][1])
  v2_l2 <- as.numeric(v2[[1]][1])
  r2_l1 <- 1 - (v2_l1 / v1_l1)
  r2_l2 <- 1 - (v2_l2 / v1_l2)
  r2_l1_2 <- 1 - ((v2_l1 + v2_l2) / (v1_l1 + v1_l2))
  r2_l2_2 <- NA
  n_harmonic <- NA
  if (!is.null(n)) {
    N <- length(as.numeric(n))
    n_harmonic <- (1 / N * sum(1 / n))^-1
    r2_l2_2 <- 1 - ((v2_l1 / n_harmonic + v2_l2) / (v1_l1 / n_harmonic + v1_l2))
  }
  cat("Multilevel R sqauered for two-level models \n")
  cat("Level 1:", round(r2_l1_2, 3), "\n")
  cat("Level 2:", round(r2_l2_2, 3), "\n")
  
  invisible(list(
      r2_l1 = r2_l1, 
      r2_l2 = r2_l2, 
      r2_l1_2 = r2_l1_2, 
      r2_l2_2 = r2_l2_2, 
      n = n_harmonic
  ))
}

