sem_table <- function(object, 
                      label = "noname", 
                      fit_indices = c("chisq", "df", "pvalue", "cfi", "tli", "aic", "bic", "rmsea", "srmr"), 
                      round = 3,
                      type = "df") {
  mat <- sapply(object, function(x) fitMeasures(x, fit_indices))
  if (type == "df")
    data.frame(label, t(round(mat, round)))
  if (type == "html") {
    knitr::kable(mat, caption = "Fit indices", align = c("l", rep("c", ncol(mat) - 1)), row.names = FALSE) %>%
      kableExtra::kable_styling(bootstrap_options = "basic", full_width = FALSE) %>%
      kableExtra::column_spec(1, bold = TRUE, color = "black") %>%
      kableExtra::row_spec(1, hline_after = TRUE) 
      #  %>%kableExtra::footnote(general = pillai)
  }
    
}
