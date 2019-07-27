sem_table <- function(object, 
                      label = "noname", 
                      fit.indices = c("chisq", "df", "pvalue", "cfi", "tli", "aic", "bic", "rmsea", "srmr"), 
                      round = 3) {
  mat <- sapply(object, function(x) fitMeasures(x, fit.indices))
  data.frame(label, t(round(mat, round)))
}
