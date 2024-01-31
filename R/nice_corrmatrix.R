#' Nice correlation matrix
#'
#' From a psych corr object it gives a correlation matrix formatted for overview
#'
#' @param cr A data frame or a correlation matrix returned by the psych package
#' @param upper TRUE if upper triangle should be included.
#' @param lower TRUE if lower triangle should be included.
#' @param digits Round to given digit position.
#' @param labels Character string. If "auto" labels are taken from a label
#'   attribute.
#' @param nsig_p p level below which correlations are considered not significant.
#' @param char_nsig Character indexing non-significant values.
#' @param char_NA Character for NA values.
#' @param char_autocor Character for diagonal (e.g. "-" or "1.00").
#' @param char_p10 Character indexing .10 significance level.
#' @param values TRUE if values should be included.
#' @param stars TRUE if stars should be included.
#' @param caption Caption for a html table.
#' @param type Character string. "df" for data-frame. "html" for html table
#'   (needs knitr and kableExtra packages).
#' @param ... Further arguments passed to the [psych::corr.test()] function.
#'
#' @return A data-frame or a html table object
#' @examples
#' nice_corrmatrix(mtcars, ci = TRUE)
#' 
#' @export

nice_corrmatrix <- function(cr, 
                            upper = FALSE, 
                            lower = TRUE,
                            digits = 2, 
                            values = TRUE, 
                            stars = TRUE, 
                            numbered_columns = TRUE,
                            descriptives = TRUE,
                            labels = NULL,
                            ci = FALSE, 
                            nsig_p = .10, 
                            char_nsig, 
                            char_autocor = "\uFF0D",
                            char_p10 = "\u271e", 
                            char_NA = "", 
                            caption = "Correlation matrix",
                            drop_zero = TRUE,
                            type = "html", 
                            file = NULL,
                            ...) {
  
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
    .n <- apply(cr, 2, function(x) sum(!is.na(x)))
    cr <- psych::corr.test(cr, adjust = "none", ...)
  }
  
  r <- cr$r
  p <- cr$p

  r <- round(r, digits)
  r <- format(r, digits = digits, nsmall = digits)

  if (!values) r[TRUE] <- ""

  if (!missing(char_nsig)) r[p > nsig_p] <- char_nsig
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
  
  if (ci) {
    break_sign <- if (identical(type, "html")) "<br>" else " "
    cis <- paste0(
      "[", formatC(cr$ci$lower, format = 'f', digits = digits), ", ", 
      formatC(cr$ci$upper, format = 'f', digits = digits), "]"
    )
    ci_names <- strsplit(rownames(cr$ci), "-")
    for(i in seq_along(cis)) {
      new_r <- paste0(r[ci_names[[i]][1], ci_names[[i]][2]], break_sign, cis[[i]])
      r[ci_names[[i]][1], ci_names[[i]][2]] <- new_r
      r[ci_names[[i]][2], ci_names[[i]][1]] <- new_r
    }
  }

  
  
  if (!upper) r[upper.tri(r)] <- ""
  if (!lower) r[lower.tri(r)] <- ""
  
  if (drop_zero) r <- gsub("0\\.", ".", r)
  
  r <- as.data.frame(r)
  
  if (!is.null(labels)) rownames(r) <- labels
  
  if (numbered_columns) {
    rownames(r) <- paste0(1:nrow(r), ". ", rownames(r))
    colnames(r) <- paste0(1:nrow(r), "") 
  }
  
  if (descriptives) {
    .varnames <- names(r)
    r$n <- .n
    r$M <- round(.means, digits)
    r$SD <- round(.sds, digits)
    r <- r[, c("n", "M", "SD", .varnames)]
  }
 
  r$Variable <- rownames(r)
  rownames(r) <- NULL
  r <- r[, c(ncol(r), 1:(ncol(r) - 1))]
  
  r <- format(r, justify = "left")
  
  r <- set_wmisc_attributes(
    r, 
    title = caption, 
    note = paste0(
      char_p10, 
      "*p* < .10; \\**p* < .05; \\*\\**p* < .01; \\*\\*\\**p* < .001"
    )
  )
  
  if (type == "df") {
    cat("Correlation matrix.\n", sep = "")
    cat("\n")
    if (stars) {
      note <- paste0(char_p10, "p<.10; *p<.05; **p<.01; ***p<.001.\n", sep = "")
      cat(note)
    }
    return(r)
  }

  if (type == "html") {
    out <- nice_table(
      r,
      #title = caption,
      #footnote = paste0(char_p10, "*p* < .10; \\**p* < .05; \\*\\**p* < .01; \\*\\*\\**p* < .001"),
      file = file
    ) |> gt::fmt_markdown(columns = 5:ncol(r))
    return(out)
    
  }
}
