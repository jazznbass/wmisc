

scaledic_to_haven <- function(df) {
  for (i in seq_along(df)) {
    df[[i]] <- scaledic_to_haven_variable(df[[i]])
  }
  df
}

scaledic_to_haven_variable <- function(x) {
  
  dic <- attr(x, "dic")
  
  if (is.null(dic)) {
    return(x)
  }
  
  if (is.null(attr(x, "label"))) {
    attr(x, "label") <- dic$item_label
  }
  if (is.null(attr(x, "labels"))) {
    attr(x, "labels") <- dic$values
  }
  x
}

