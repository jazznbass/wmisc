hist_levels <- function(VAR, LAB, dat1, dat2, grouping, mfrow = c(2, 2), ...) {
  cat("Classification by ", grouping, "\n")
  n.var <- length(VAR)
  av <- as.matrix(cbind(dat1[, VAR]))
  UV <- factor(dat1[, grouping])
  res1 <- aggregate(av ~ UV, data = dat1, FUN = mean, na.rm = TRUE)
  av <- as.matrix(cbind(dat2[, VAR]))
  UV <- factor(dat2[, grouping])
  res2 <- aggregate(av ~ UV, data = dat2, FUN = mean, na.rm = TRUE)
  res <- merge(res1, res2, by = "UV")
  res2 <- res[, 1:(n.var + 1)]
  for (i in 2:(n.var + 1)) {
    res2[i] <- res[i] - res[i + n.var]
  }
  op <- par(mfrow = mfrow)
  for (i in 2:(n.var + 1)) {
    hist(res2[, i], main = LAB[i - 1], labels = TRUE, xlab = "d", ylab = "Frequency", col = "grey", ...)
    abline(v = c(-0.5, 0, 0.5), lty = c(2, 1, 2))
  }
  par(op)
}
