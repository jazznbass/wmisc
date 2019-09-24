anova_table <- function(test, labels = NULL, type = "df") {

  if (is.null(labels)) labels <- paste0("Model ",1:length(test[[1]]))
  out <- tibble(
    Variable = labels, 
    df = test[[3]], 
    AIC = round(test[[4]], 0), 
    BIC = round(test[[5]], 0), 
    logLik = round(test[[6]], 1), 
    L = round(test[[8]], 1), 
    p = round(test[[9]], 2)
  )
  if (type == "df") out
  
}
