#' Creates a t.test table for multiple dependent variables
#'
#' @param dv A data frame with the dependent variables or a character vector
#'   with variable names when data is defined.
#' @param iv A data frame or vector with the independent variable or a character
#'   if data is defined.
#' @param data A data frame.
#' @param conditions A character vector of length two with the names of the two
#'   conditions. Defaults to the first two levels of the independent variable
#'   'iv' if applicable.
#' @param labels A character vector of length two with labels for the dependent
#'   variables.
#' @param concise A more concise table with mean and SD in one column.
#' @param nice_p If TRUE, p values are printed in a nice format.
#' @param digits Number of digits for rounding mean and SD values
#' @param var_equal If FALSE, a t-test for unequal variances is calculated.
#' @param order Either "12" or "21" depicting whether group two is compared to
#'   group one or vice versa.
#' @param type Either "df" for data frame or "html" for html table.
#' @param caption Table caption is type = "html"
#' @param bootstrap_options see kable_styling()
#' @param full_width see kable_styling()
#'
#' @return A tibble or a kableExtra object
#' @export
#'
#' @examples
#' dv <- data.frame(
#'   a = c(rnorm(85, 50, 10), rnorm(200, 70, 20)),
#'   b = c(rnorm(85, 50, 10), rnorm(200, 55, 20)),
#'   iv = factor(c(rep("A", 85), rep("B", 200)))
#' )
#'
#' t_test_table(
#'   c("a", "b"), "iv", data = dv, 
#'   labels = c("Motivation", "Achievement")
#' )

t_test_table <- function(dv, 
                         iv, 
                         data, 
                         method = "cohen",
                         conditions = levels(factor(iv))[order], 
                         labels = NULL, 
                         concise = TRUE, 
                         nice_p = TRUE, 
                         digits = 1, 
                         var_equal = FALSE, 
                         label_attr = TRUE,
                         manova = TRUE, 
                         order = 1:2, 
                         type = "html",
                         caption = "t-test table", 
                         bootstrap_options = c("condensed", "striped", "hover"), 
                         full_width = TRUE) {

  if (!missing(data)) {
    dv <- data[, dv]
    iv <- data[[iv]]
  }
  dv <- dv[iv %in% conditions, ]
  iv <- iv[iv %in% conditions]
  iv <- factor(iv, levels = levels(iv)[order])
  lev <- levels(iv)

  out <- tibble(
    Scale = character(), M1 = numeric(), M2 = numeric(), SD1 = numeric(),
    SD2 = numeric(), d = numeric(), t = numeric(), df = numeric(),
    p = numeric(), n1 = numeric(), n2 = numeric()
  )

  if (is.null(labels)) labels <- names(dv)
  
  for (i in 1:ncol(dv)) {
    res <- t.test(dv[[i]] ~ iv, var.equal = var_equal)
    sds <- aggregate(dv[[i]], by = list(iv), sd, na.rm = TRUE)[,2]
    vars <- aggregate(dv[[i]], by = list(iv), var, na.rm = TRUE)[,2]
    ns <- aggregate(dv[[i]], by = list(iv), function(x) sum(!is.na(x)))[,2]
 
    if (label_attr && !is.null(attr(dv[[i]], "label"))) 
      labels[i] <- attr(dv[[i]], "label")
    out[i, "SD1"] <- sds[1]
    out[i, "SD2"] <- sds[2]
    out[i, "p"] <- res$p.value
    out[i, "df"] <- res$parameter
    out[i, "t"] <- res$statistic
    out[i, "M1"] <- res$estimate[1]
    out[i, "M2"] <- res$estimate[2]
    out[i, "Scale"] <- labels[i]
    sd <- switch(method,
      "glass" = sds[2],
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
    
    out[i, "d"] <- (res$estimate[1] - res$estimate[2]) / sd
    
    if (method == "hedges") {
      out[i, "d"] <- (1 - (3) / (4 * (ns[1] + ns[2]) - 9)) * out[i, "d"]
    }
    
    out[i, "n1"] <- ns[1]
    out[i, "n2"] <- ns[2]
  }
  
  round_ <- function(x, digits) {
    format(round(x, digits = digits), nsmall = digits) 
  }

  out[i, "p"] <- round(out$p[i], digits)
  
  out <- out %>%
    mutate_at(c(2:5), round_, digits = digits) %>%
    mutate_at("d", round_, digits = digits) %>%
    mutate_at("df", round_, 1) %>%
    mutate_at("t", round_, 2)
  

  colnames(out)[2:3] <- paste0("M ", lev)
  colnames(out)[4:5] <- paste0("SD ", lev)
  colnames(out)[10:11] <- paste0("n ", lev)
  if (concise) {
    MS_A <- paste0(out[[2]], " (", out[[4]], ")")
    MS_B <- paste0(out[[3]], " (", out[[5]], ")")
    out <- 
      tibble(Scale = out[[1]], MS_A, MS_B) %>%
        bind_cols(out[c(6:ncol(out))])
    colnames(out)[2:3] <- paste0("M (SD) ", lev)
  }

  if (nice_p) out$p <- nice_p(out$p, digits = 2)

  if(type == "html") {
    out <- out %>% 
      knitr::kable(
        caption = caption, 
        align = c("l", rep("c", ncol(out))), 
        row.names = FALSE
      ) %>%
      kableExtra::kable_classic(
        bootstrap_options = bootstrap_options, full_width = full_width
      )
    
    note <- paste0("Method for estimating standard deviation: ", method)
    
    if (manova) {
      res <- lm(as.matrix(dv) ~ iv) %>%
        manova() %>%
        summary() %>%
        .$"stats"
      
      note <- paste0(note, "; ", sprintf(
        "Manova: Pillai = %.2f; F(%d, %d) = %.2f; p = %.3f", 
        res[1, 2], res[1, 4], res[1, 5], res[1, 3], res[1, 6]
      ))
  

    }  

    out <- kableExtra::footnote(out, general = note)
    
    return(out)
  }
    
  out
}
