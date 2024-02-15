a <- c("(Intercept) id_subject", "(Intercept) id_subject:Time", "(Intercept) id_subject:Effect", 
  "ICC", "Residual", "Observations", "R² Conditional", "R² Marginal"
)

end <- vars_end <- c(
  "Residual", "ICC", 
  "R\u00b2 Conditional", "R\u00b2 Marginal",
  "N", "Observations"
)

shift_to <- function(a, end = NULL, front = NULL) {
  
  id_front <- sapply(id_front, \(x) which(a %in% x)) |> as.numeric()
  id_front <- id_front[!is.na(id_front)]    
  id_end <- sapply(end, \(x) which(a %in% x)) |> as.numeric()
  id_end <- id_end[!is.na(id_end)]  
  
  c(a[if_front], a[-c(id_front, id_end)], a[id_end])
}

