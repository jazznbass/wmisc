#' Nice correlationmatrix
#'
#' From a psych corr object it gives a correlationmatrix formated for overview
#'
#' @param cr A data frame or a correlation matrix returned by the psych package
#' @param upper TRUE if upper triangle should be included
#' @param lower TRUE if lower triangle should be included
#' @param alpha Alpha level
#' @param digits Round to given digit position
#' @param labels Character string. If "auto" labels are taken from a 
#'   label attribute.
#' @param char_nsig Character indexing non-significant values
#' @param char_NA Character for NA values
#' @param char_autocor Character for diagonal (e.g. "-" or "1.00")
#' @param char_p10 Character indexing .10 significance level
#' @param values TRUE if values should be included
#' @param stars TRUE if stars should be included
#' @param caption Caption for a html table.

#' @param type Character string. "df" for data-frame. "html" for html table (needs knitr and kableExtra packages)
#'
#' @return A data-frame or a html table object
#' @export

nice_corrmatrix <- function(cr, upper = TRUE, lower = TRUE,
                            alpha = .10, digits = 2, 
                            values = TRUE, stars = TRUE, 
                            numbered_columns = FALSE,
                            descriptives = TRUE,
                            labels = NULL,
                            char_nsig, char_autocor = "-",
                            char_p10 = "\u271d", char_NA = "", 
                            caption = "Correlation matrix.",
                            drop_zero = TRUE,
                            type = "df") {
  
  if (inherits(cr, "data.frame")) {
    
    if (identical(labels, "auto")) {
      labels <- map2_chr(
        cr, 
        names(cr), 
        function(.x, .y) { 
          if(!is.null(attr(.x, "label"))) attr(.x, "label") else .y
        }
      )    
    }
    
    .means <- apply(cr, 2, function(x) mean(x, na.rm = TRUE))
    .sds <- apply(cr, 2, function(x) sd(x, na.rm = TRUE))
    cr <- psych::corr.test(cr)
  }
  
  r <- cr$r
  p <- cr$p

  r <- format(r, digits = digits, nsmall = digits)

  if (!values) r[TRUE] <- ""

  if (!missing(char_nsig)) r[p > alpha] <- char_nsig
  r[is.na(cr$r)] <- char_NA
  diag(r) <- char_autocor
  diag(p) <- 1
  if (stars) {
    copy_r <- r
    r[which(p <= .10)] <- paste0(copy_r[which(p <= .10)], char_p10)
    r[which(p <= .05)] <- paste0(copy_r[which(p <= .05)], "*  ")
    r[which(p <= .01)] <- paste0(copy_r[which(p <= .01)], "** ")
    r[which(p <= .001)] <- paste0(copy_r[which(p <= .001)], "***")
    r[which(p > .10 & p < 1)] <- paste0(copy_r[which(p > .10 & p < 1)], "   ")
  }
  
  r[which(cr$r >= 0)] <- paste0(" ", r[which(cr$r >= 0)])
  
  if (!upper) r[upper.tri(r)] <- ""
  if (!lower) r[lower.tri(r)] <- ""
  
  if (drop_zero) r <- gsub("0\\.", ".", r)
  
  r <- as.data.frame(r)
  
  if (!is.null(labels)) rownames(r) <- labels
  
  if (numbered_columns) {
    rownames(r) <- paste0(1:nrow(r), " ", rownames(r))
    colnames(r) <- paste0(1:nrow(r), "") 
  }
  
  if (descriptives) {
    .varnames <- names(r)
    r$M <- round(.means, digits)
    r$SD <- round(.sds, digits)
    r <- r[, c("M", "SD", .varnames)]
  }
 
  if (type == "df") {
   cat(
     "Correlation matrix.\n",
     "Probability signs above the diagonal are adjusted for multiple tests.\n",
     sep = ""
   )
   if (stars) cat(char_p10, "p<.10; *p<.05; **p<.01; ***p<.001.\n", sep = "")
   cat("\n")
   r <- format(r, justify = "left")
   return(r)
 }

  if (type == "html") {
    knitr::kable(
      r, format = "html", caption = caption, row.names = TRUE, align = "c"
    ) %>%
      kableExtra::kable_styling(
        bootstrap_options = "basic", full_width = FALSE
      ) %>%
      kableExtra::column_spec(1, bold = TRUE, color = "black") %>%
      kableExtra::row_spec(1, hline_after = TRUE) %>%
      kableExtra::footnote(
        general = paste0(
          char_p10, 
          "p < .10; \\*p < .05; \\*\\*p < .01; \\*\\*\\*p < .001")
        ) %>%
      return()
  }
}
