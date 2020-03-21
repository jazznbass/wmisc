#' Creats a chi.sqaured test tabel for multiple dependent variables
#'
#' @param dv A data frame with the dependent variables or a character vector when argument data is set.
#' @param iv A data frame or vector with the independent variable or a character if argument data is set.
#' @param data A data frame.
#' @param conditions A character vectot of length two with the names of the two conditions.
#' Defaults to the first two levels of the independent variable 'iv' if applicable.
#' @param ref_levels Vector with the reference level for each dependent variable. Values are repeated if length of levels vector is smaller than number dependent variables.
#' @param labels A character vector of length two with labels for the dependent variables.
#' @param nice_p If TRUE, p values are printed in a nice format.
#' @param digits Number of digits for rounding mean and sd values
#' @param order Either "12" or "21" depicting whether group two is compared to group one or vice versa.
#' @param type Either "df" for data frame or "html" for html table.
#' @param caption Tabel caption is type = "html"
#' @param bootstrap_options see kable_styling()
#' @param full_width see kable_styling()
#'
#' @return A tibble or a kableExtra object
#' @export
#'
#' @examples
#' dat <- data.frame(
#'   a = factor(rbinom(100, 1, 0.3), labels = c("m", "f")),
#'   b = factor(rbinom(100, 1, 0.3), labels = c("no", "yes")),
#'   g = factor(rbinom(100, 1, 0.5), labels = c("no_sen", "sen"))
#'  )
#' chi_test_table(c("a", "b"), "g", data = dat, nice_p = TRUE, ref_levels = c("m", "no"))
chi_test_table  <- function(dv, iv, data, conditions = levels(factor(iv))[1:2], 
                         ref_levels = 2,
                         labels = NULL, 
                         nice_p = TRUE, digits = 2, 
                         order = "12", 
                         label_attr = TRUE, 
                         type = "df", 
                         caption = "X-sqaured-test table", 
                         bootstrap_options = c("condensed", "striped", "hover"), 
                         full_width = TRUE) {

  if (!missing(data)) {
    dv <- data[, dv, drop = FALSE]
    iv <- data[[iv]]
  }
  dv <- dv[iv %in% conditions,]
  iv <- iv[iv %in% conditions]
  iv <- factor(iv)
  lev <- levels(iv)
  ref_levels <- rep_len(ref_levels, ncol(dv))
  out <- tibble(
    Scale = character(), Level = character(), P1 = numeric(), P2 = numeric(), d = numeric(), 
    X = numeric(), df = numeric(), p = numeric(), n1 = numeric(), n2 = numeric()
  )

  if (is.null(labels)) labels <- names(dv)
  
  for (i in 1:ncol(dv)) {
    if (label_attr && !is.null(attr(dv[[i]], "label"))) labels[i] <- attr(dv[[i]], "label")
    l <- ref_levels[i]
    if (is.numeric(l)) l <- levels(dv[[i]])[l]
    if (!l %in% levels(dv[[i]])) stop("Wrong levels.")
    dv[[i]] <- relevel(dv[[i]], l)
    res <- chisq.test(dv[[i]], iv)
    out[i, "Level"] <- l
    out[i, "p"] <- res$p.value
    out[i, "df"] <- res$parameter
    out[i, "X"] <- res$statistic
    out[i, "P1"] <- res$observed[1, 1] / sum(res$observed[,1])
    out[i, "P2"] <- res$observed[1, 2] / sum(res$observed[,2])
    out[i, "Scale"] <- labels[i]
    out[i, "d"] <- abs(out[i, "P1"] - out[i, "P2"])
    out[i, "n1"] <- sum(!is.na(dv[iv == conditions[1], i]))
    out[i, "n2"] <- sum(!is.na(dv[iv == conditions[2], i]))
  }
  
  round_ <- function(x, digits) {
    format(round(x, digits=digits), nsmall = digits) 
  }
  
  out <- out %>%
    mutate_at(c(3:6), round_, digits = digits)
  
  if (order == "21") {
    out$t <- out$X * -1
  }
  colnames(out)[3:4] <- paste0("Prop ", lev)
  colnames(out)[6] <- "X Squared"
  colnames(out)[5] <- "difference"
  colnames(out)[9:10] <- paste0("n ", lev)
  
  if (nice_p) out$p <- nice_p(out$p) else out$p <- round_(out$p, 3)

  if(type == "html") {
    out <- knitr::kable(out, caption = caption, align = c("l", rep("c", 8)), row.names = FALSE) %>%
      kableExtra::kable_styling(bootstrap_options = bootstrap_options, full_width = full_width) %>%
      kableExtra::column_spec(1, bold = TRUE, color = "black") %>%
      kableExtra::row_spec(1, hline_after = TRUE)
    
    
    return(out)
  }
    
  out
}
