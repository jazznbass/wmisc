### Multilevel (2 Levels) R.squared change
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
  cat(
    "(Snijders, T. A. B., & Bosker, R. J. (1994). ",
    "Modeled Variance in Two-Level Models.",
    "Sociological Methods & Research, 22(3), 342-363.)\n\n"
  )
  
  cat("Level 1:", R2.L1.2, "\n")
  cat("Level 2:", R2.L2.2, "\n")
  
  invisible(c(R2.L1 = R2.L1, R2.L2 = R2.L2, R2.L1.2 = R2.L1.2, R2.L2.2 = R2.L2.2, n = n.harmonic))
}

