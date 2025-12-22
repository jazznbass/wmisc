#' Creates a t-test table for multiple dependent variables
#'
#' @param dv A data frame with the dependent variables or a character vector
#'   with variable names when data is defined.
#' @param iv A data frame or vector with the independent variable or a character
#'   if data is defined.
#' @param data A data frame.
#' @param method Either `"cohen"`, `"glass"`, or `"hedges"`.
#' @param conditions A character vector of length two with the names of the two
#'   conditions. Defaults to the first two levels of the independent variable
#'   `'iv'` if applicable.
#' @param labels A character vector of length two with labels for the dependent
#'   variables.
#' @param labels_conditions Vector of length 2 which resets condition labels.
#' @param concise A more concise table with mean and SD in one column.
#' @param nice_p If TRUE, p values are printed in a nice format.
#' @param digits Number of digits for rounding mean and SD values
#' @param var_equal If FALSE, a t-test for unequal variances is calculated.
#' @param type Either "df" for data frame or "html" for html table.
#' @param caption Table caption is type = "html"
#'
#' @return A data.frame or an html table
#' @export
#'
#' @examples
#' nice_t_test_table(
#'   data = mtcars, 
#'   iv = "am", 
#'   dv = c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "gear", "carb")
#' )
#' 
#' df <- data.frame(
#'   a = c(rnorm(85, 50, 10), rnorm(200, 70, 20)),
#'   b = c(rnorm(85, 50, 10), rnorm(200, 55, 20)),
#'   iv = factor(c(rep("Regular", 85), rep("Special", 100), rep("Restricted", 100)))
#' )
#' 
#' nice_t_test_table(
#'   c("a", "b"), "iv", 
#'   data = df,
#'   conditions = c("Restricted","Special"), 
#'   labels = c("Motivation", "Achievement")
#' )

nice_t_test_table <- function(dv, 
                              iv, 
                              data, 
                              method = "cohen",
                              conditions = NULL, 
                              labels = NULL,
                              labels_conditions = NULL,
                              concise = TRUE, 
                              nice_p = TRUE, 
                              digits = 1, 
                              var_equal = FALSE, 
                              label_attr = TRUE,
                              manova = TRUE, 
                              type = "html",
                              caption = NULL,
                              alternative = "two.sided",
                              file = NULL) {

  on.exit(print_messages())
  
  if (!missing(data)) {
    if (!is.character(iv)) {
      add_message("iv should be a character string when data are provided")
    }
    dv <- data[, dv, drop = FALSE]
    iv <- data[[iv]]
  }
  
  if (is.null(conditions)) conditions <- levels(factor(iv))
  if (is.null(labels_conditions)) labels_conditions <- conditions
  
  dv <- dv[iv %in% conditions, , drop = FALSE]
  iv <- iv[iv %in% conditions]
  iv <- factor(iv, levels = conditions, labels = labels_conditions)
  
  if (is.null(caption)) {
    caption <- paste0("T-test table comparing conditions ", 
                      paste0(labels_conditions, collapse = " vs. "),
                      " in variable ", as.character(match.call()[3]))
  }
  
  out <- data.frame(
    Scale = character(), 
    n1 = numeric(), n2 = numeric(),
    M1 = numeric(), M2 = numeric(), 
    SD1 = numeric(), SD2 = numeric(), 
    d = numeric(), 
    t = numeric(), df = numeric(), p = numeric()
  )

  if (is.null(labels)) labels <- names(dv)

  for (i in 1:ncol(dv)) {
    res <- t.test(
      dv[[i]] ~ iv, 
      var.equal = var_equal, 
      alternative = alternative
    )
    sds <- aggregate(dv[[i]], by = list(iv), sd, na.rm = TRUE)[,2]
    vars <- aggregate(dv[[i]], by = list(iv), var, na.rm = TRUE)[,2]
    ns <- aggregate(dv[[i]], by = list(iv), function(x) sum(!is.na(x)))[,2]
 
    label <- get_label(dv[[i]])
    if (label_attr && !is.null(label)) labels[i] <- label
    out[i, "SD1"] <- sds[1]
    out[i, "SD2"] <- sds[2]
    out[i, "p"] <- res$p.value
    out[i, "df"] <- res$parameter
    out[i, "t"] <- res$statistic*-1
    out[i, "M1"] <- res$estimate[1]
    out[i, "M2"] <- res$estimate[2]
    out[i, "Scale"] <- labels[i]
    sd <- switch(method,
      "glass" = sds[1],
      #"cohen" = sqrt((vars[1] + vars[2]) / 2),
      "cohen" = sqrt(
        ((ns[1] - 1) * vars[1] + (ns[2] - 1) * vars[2]) / 
        (ns[1] + ns[2] - 2)
      ),
      "hedges" = sqrt(
        ((ns[1] - 1) * vars[1] + (ns[2] - 1) * vars[2]) / 
          (ns[1] + ns[2] - 2)
      )
    )
    
    out[i, "d"] <- (res$estimate[2] - res$estimate[1]) / sd
    
    if (method == "hedges") {
      out[i, "d"] <- (1 - (3) / (4 * (ns[1] + ns[2]) - 9)) * out[i, "d"]
    }
    
    out[i, "n1"] <- ns[1]
    out[i, "n2"] <- ns[2]
  }
  
  colnames(out)[4:5] <- paste0("M ", labels_conditions)
  colnames(out)[6:7] <- paste0("SD ", labels_conditions)
  colnames(out)[2:3] <- paste0("n ", labels_conditions)
  if (concise) {
    out[, 4:7] <- round(out[, 4:7], digits)
    MS_A <- paste0(out[[4]], " (", out[[6]], ")")
    MS_B <- paste0(out[[5]], " (", out[[7]], ")")
    
    out <- data.frame(
      Scale = out[[1]],
      nA = out[[2]],
      nb = out[[3]],
      MS_A, MS_B
    ) |> dplyr::bind_cols(out[c(8:ncol(out))])
    colnames(out)[4:5] <- paste0("M (SD) ", labels_conditions)
  }

  if (nice_p) out$p <- nice_p(out$p, digits = 2)

  note <- paste0("Method for estimating d: ", method)
  
  if (manova) {
    res <- lm(as.matrix(dv) ~ iv)  |> 
      manova()  |> 
      summary()
    res <- res$"stats"
    note <- paste0(note, "; ", sprintf(
      "Manova: Pillai = %.2f; F(%d, %d) = %.2f; p = %.3f", 
      res[1, 2], res[1, 4], res[1, 5], res[1, 3], res[1, 6]
    ))
  }  
  
  out <- set_wmisc_attributes(out, 
    title = caption,
    note = note
  )
  
  if(type == "html") {
    names(out)[4:5] <- labels_conditions
    names(out)[2:3] <- paste0(" ", labels_conditions, " ")

    out <- set_wmisc_attributes(out,
      spanner = list("M (SD)" = 4:5, "N" = 2:3),
      file = file
    )
    out <- nice_table(out, digits = digits, round = digits)
  }
    
  
  
  out
}

#' @export
#' @rdname nice_t_test_table
t_test_table <- function(...) nice_t_test_table(...)

  
