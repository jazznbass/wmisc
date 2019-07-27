#' Nice correlationmatrix
#'
#' From a psych corr object it gives a correlationmatrix formated for overview
#'
#' @param cr A correlation matrix returned by the psych package
#' @param upper TRUE if upper triangle should be included
#' @param lower TRUE if lower triangle should be included
#' @param alpha Alpha level
#' @param digits Round to given digit position
#' @param n.sig Charcter indexing non-significant values
#' @param values TRUE if values should be included
#' @param stars TRUE if stars should be included
#' @param autocor Character for diagonal (e.g. "-" or "1.00")
#' @param square Report squared r instead of r
#' @param caption Caption for a html table.
#' @param sig.10 Character indexing .10 sognificance level
#' @param type Charcater string. "df" for data-frame. "html" for html table (needs knitr and kableExtra packages)
#'
#' @return A data-frame or a html table object
#' @export

nice_corrmatrix <- function(cr, upper = TRUE, lower = TRUE,
                            alpha = .10, digits = 2, n.sig,
                            values = TRUE, stars = TRUE, autocor = "-",
                            square = FALSE, caption = "Correlation matrix.",
                            sig.10 = "\u271d  ", type = "df") {
  r <- as.data.frame(cr$r)
  p <- as.data.frame(cr$p)
  copt <- r
  if (square) r <- r * r
  r <- round(r, digits)
  if (!values) r[TRUE] <- ""

  if (!missing(n.sig)) r[p > alpha] <- n.sig
  if (stars) {
    copt[p <= .10] <- paste0(r[p <= .10], sig.10)
    copt[p <= .05] <- paste0(r[p <= .05], "*  ")
    copt[p <= .01] <- paste0(r[p <= .01], "** ")
    copt[p <= .001] <- paste0(r[p <= .001], "***")
    copt[r == 1] <- paste0(autocor, "    ")
    copt[p > .10] <- paste0(r[p > .10], "  ")
    r <- copt
  }

  if (!upper) r[upper.tri(r)] <- ""
  if (!lower) r[lower.tri(r)] <- ""
  if (type == "df") {
    cat("Correlation matrix.\nProbability signs above the diagonal are adjusted for multiple tests.\n")
    if (square) cat("Explained variance r-squared\n")
    if (stars) cat(sig.10, "p<.10; *p<.05; **p<.01; ***p<.001.\n")
    cat("\n")
    return(r)
  }

  if (type == "html") {
    kable(r, caption = caption, row.names = TRUE) %>%
      kable_styling(bootstrap_options = "basic", full_width = FALSE) %>%
      column_spec(1, bold = TRUE, color = "black") %>%
      row_spec(1, hline_after = TRUE) %>%
      footnote(general = paste0(sig.10, "p<.10; *p<.05; **p<.01; ***p<.001")) %>%
      return()
  }
}
