anova_table <- function(test, labels = NULL) {
  # test <- anova(...)
  if (is.null(labels)) {
    labels <- 1:length(test[[1]])
  }
  out <- data.frame(Variable = labels, df = test[[3]], AIC = round(test[[4]], 0), BIC = round(test[[5]], 0), logLik = round(test[[6]], 1), L = round(test[[8]], 1), p = round(test[[9]], 2))
  list(table = out)
}
