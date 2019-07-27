########################## eRm package extensions

lrDIF <- function(lr, cut = 1.0) {
  eta1 <- lr$etalist[[1]]
  eta2 <- lr$etalist[[2]]
  na <- names(eta1)
  DIF <- as.data.frame(round(eta2 - eta1, 2))
  names(DIF) <- "DIF"
  cu <- rep("", length(eta1))
  if (sum(abs(DIF$DIF) >= cut) > 0) {
    cu[abs(DIF$DIF) >= cut] <- "*"
  }
  DIF$CUT <- cu
  cat("Positiv: more difficult for Group 2\nNegative: easier for Group 2.\n\n")
  # cr <- table(DIF$CUT)[[2]]
  cr <- sum(abs(DIF$DIF) >= cut)
  
  cat(sprintf("%d critical items (%d%%) with a cut-off criteria of %.2f", cr, round(cr / (nrow(DIF)) * 100), cut), "\n\n")
  DIF
}

plotWALD <- function(wald, ...) {
  ct <- as.data.frame(wald$coef.table)
  ct <- ct[order(ct$"z-statistic"), ]
  row.names(ct) <- substring(row.names(ct), 6)
  
  select <- ct[ct$"p-value" <= 0.05, ]
  select <- select[order(row.names(select)), ]
  select$"p-value" <- round(select$"p-value", 3)
  
  plot(ct, ylab = "p", type = "b", main = "Wald test", ...)
  abline(h = 0.05, col = "red")
  text(select, row.names(select), cex = 0.75)
  
  cat("Plot produced ...\n")
  cat(nrow(select), "critical items and ", nrow(ct) - nrow(select), " nonctitical items.\n")
  stay <- ct[ct$"p-value" > 0.05, ]
  li <- sort(row.names(stay))
  out <- paste("\"", li, "\", ", sep = "", collapse = "")
  out <- paste("c(", substr(out, start = 1, stop = nchar(out) - 2), ")", sep = "", collapse = "")
  cat("Syntax for included items: \n", out, "\n")
  cat("Critical Items:\n\n")
  
  select
}

personfit <- function(pp) {
  chi <- as.numeric(pp$p.fit)
  outfitMSQ <- as.numeric(pp$p.outfitMSQ)
  infitMSQ <- as.numeric(pp$p.infitMSQ)
  id <- 1:length(pp$p.fit)
  d.f <- as.numeric(pp$p.df - 1)
  p <- 1 - pchisq(chi, d.f)
  pp.df <- data.frame(id, chi, df = d.f, p.value = round(p, 4), outfitMSQ, infitMSQ)
  out.df <- pp.df[outfitMSQ > 2 | infitMSQ > 2, ]
  pp.df <- pp.df[outfitMSQ <= 2 & infitMSQ <= 2, ]
  STAY <- as.numeric(pp.df$id)
  OUT <- as.numeric(out.df$id)
  cat("Total persons: ", length(id), ", to be excluded: ", length(id) - length(STAY), " = ", round((length(id) - length(STAY)) / length(id) * 100), "%\n")
  re <- list()
  re$exclude <- OUT
  re$include <- STAY
  re
}


itemfit <- function(ip) {
  chi <- as.numeric(ip$i.fit)
  outfitMSQ <- as.numeric(ip$i.outfitMSQ)
  infitMSQ <- as.numeric(ip$i.infitMSQ)
  id <- names(ip$i.fit)
  d.f <- as.numeric(ip$i.df - 1)
  p <- 1 - pchisq(chi, d.f)
  ip.df <- data.frame(id, chi, df = d.f, p.value = round(p, 4), outfitMSQ, infitMSQ)
  out.df <- ip.df[outfitMSQ > 2 | infitMSQ > 2, ]
  ip.df <- ip.df[outfitMSQ <= 2 & infitMSQ <= 2, ]
  OUT <- as.character(out.df$id)
  STAY <- as.character(ip.df$id)
  cat("Total items: ", length(id), ", to be excluded: ", length(id) - length(STAY), " = ", round((length(id) - length(STAY)) / length(id) * 100), "%\n")
  re <- list()
  re$exclude <- OUT
  re$include <- STAY
  re
}


LR.pcm.rsm <- function(PCM, RSM) {
  ### nach Mair+(2010)
  lr <- 2 * (PCM$loglik - RSM$loglik)
  df <- PCM$npar - RSM$npar
  pvalue <- 1 - pchisq(lr, df)
  cat("LR statistic: ", lr, " df =", df, " p =", pvalue, "\n")
}

LR.eRm <- function(model1, model2) {
  lr <- 2 * (model1$loglik - model2$loglik)
  df <- model1$npar - model2$npar
  pvalue <- 1 - pchisq(lr, df)
  
  main <- paste(as.character(model1$call)[1], " vs. ", as.character(model2$call)[1], collapse = "")
  xlab <- paste(as.character(model1$call)[1], " rescaled", collapse = "")
  ylab <- paste(as.character(model2$call)[1], " rescaled", collapse = "")
  
  plot(model1$betapar, model2$betapar, main = main, xlab = xlab, ylab = ylab, xlim = c(-4, 4), ylim = c(-4, 4))
  lines(c(-4, 4), c(-4, 4))
  abline(h = 0)
  abline(v = 0)
  
  cat("Andersen Likelihood Ratio Test:\n\n")
  cat("Model1: LR = ", model1$loglik, ", df = ", model1$npar, ", Modeltype: ", as.character(model1$call)[1], "\n")
  cat("Model2: LR = ", model2$loglik, ", df = ", model2$npar, ", Modeltype: ", as.character(model2$call)[1], "\n\n")
  cat("Chi-sqaured = ", lr, ", df =", df, ", p =", pvalue, "\n")
}
