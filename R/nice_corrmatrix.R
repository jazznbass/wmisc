#' Nice correlationmatrix
#'
#' From a psych corr object it gives a correlationmatrix formated for overview
#'
#' @param cr A data frame or a correlation matrix returned by the psych package
#' @param upper TRUE if upper triangle should be included
#' @param lower TRUE if lower triangle should be included
#' @param alpha Alpha level
#' @param digits Round to given digit position
#' @param n_sig Charcter indexing non-significant values
#' @param values TRUE if values should be included
#' @param stars TRUE if stars should be included
#' @param autocor Character for diagonal (e.g. "-" or "1.00")
#' @param square Report squared r instead of r
#' @param caption Caption for a html table.
#' @param sig.10 Character indexing .10 significance level
#' @param type Charcater string. "df" for data-frame. "html" for html table (needs knitr and kableExtra packages)
#'
#' @return A data-frame or a html table object
#' @export

nice_corrmatrix <- function(cr, upper = TRUE, lower = TRUE,
                            alpha = .10, digits = 2, n_sig,
                            values = TRUE, stars = TRUE, autocor = "-",
                            square = FALSE, caption = "Correlation matrix.",
                            sig.10 = "\u271d", type = "df",
                            numbered_columns = FALSE,
                            descriptives = TRUE) {
  
  if ("data.frame" %in% class(cr)) {
    .means <- apply(cr, 2, function(x) mean(x, na.rm = TRUE))
    .sds <- apply(cr, 2, function(x) sd(x, na.rm = TRUE))
    cr <- psych::corr.test(cr)
  }
  
  r <- cr$r
  p <- cr$p
  if (square) r <- r * r
  r <- format(round(r, digits = digits), nsmall = digits)  #round(r, digits)
  copt <- r
  if (!values) r[TRUE] <- ""

  if (!missing(n_sig)) r[p > alpha] <- n_sig
  #copt[copt == 1] <- paste0(autocor, "    ")
  #copt[copt == 1] <- paste0(autocor, "    ")
  diag(copt) <- paste0(autocor, "")
  diag(p) <- 1
  if (stars) {
    copt[which(p <= .10)] <- paste0(r[which(p <= .10)], sig.10)
    copt[which(p <= .05)] <- paste0(r[which(p <= .05)], "*")
    copt[which(p <= .01)] <- paste0(r[which(p <= .01)], "**")
    copt[which(p <= .001)] <- paste0(r[which(p <= .001)], "***")
    copt[which(p > .10 & p < 1)] <- paste0(r[which(p > .10 & p < 1)], "")
    r <- copt
  }
  
  if (!upper) r[upper.tri(r)] <- " "
  if (!lower) r[lower.tri(r)] <- " "
  
  r <- as.data.frame(r)
  
  if (numbered_columns) {
    rownames(r) <- paste0("", 1:nrow(r), " ", rownames(r))
    colnames(r) <- paste0("", 1:nrow(r), "") 
  }
  
  if (descriptives) {
    .varnames <- names(r)
    r$M <- round(.means, digits)
    r$SD <- round(.sds, digits)
    r <- r[, c("M", "SD", .varnames)]
  }
  
  if (type == "df") {
    cat("Correlation matrix.\nProbability signs above the diagonal are adjusted for multiple tests.\n")
    if (square) cat("Explained variance r-squared\n")
    if (stars) cat(sig.10, "p<.10; *p<.05; **p<.01; ***p<.001.\n", sep = "")
    cat("\n")
    r <- format(r, justify = "left")
    return(r)
  }

  if (type == "html") {
    knitr::kable(r, format = "html", caption = caption, row.names = TRUE, align = "c") %>%
      kableExtra::kable_styling(bootstrap_options = "basic", full_width = FALSE) %>%
      kableExtra::column_spec(1, bold = TRUE, color = "black") %>%
      kableExtra::row_spec(1, hline_after = TRUE) %>%
      kableExtra::footnote(general = paste0(sig.10, "p < .10; *p < .05; **p < .01; ***p < .001")) %>%
      return()
  }
}
