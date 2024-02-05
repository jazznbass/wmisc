#' Nice correlation matrix
#'
#' From a data.frame object it gives a correlation matrix formatted for overview
#'
#' @param cr A data frame
#' @param upper TRUE if upper triangle should be included.
#' @param lower TRUE if lower triangle should be included.
#' @param digits Round to given digit position.
#' @param labels Character string. If "auto" labels are taken from a label
#'   attribute.
#' @param show_ci If TRUE, confidence intervals are added.
#' @param nsig_p p level below which correlations are considered not
#'   significant.
#' @param char_nsig Character indexing non-significant values.
#' @param char_NA Character for NA values.
#' @param char_autocor Character for diagonal (e.g. "-" or "1.00").
#' @param char_p10 Character indexing .10 significance level.
#' @param string_ci Character string in glue format for confindence internvals.
#' @param string_p Character string in glue format for p-values.
#' @param show_r TRUE if r-values should be included.
#' @param show_p TRUE if r-values should be included.
#' @param show_stars TRUE if stars should be included.
#' @param discriptives If TRUE, mean and sd columns are added.
#' @param drop_zero If TRUE, leadning zeros are dropped.
#' @param caption Caption for an html table.
#' @param file If TRUE or a filename is provided, a file is exportet (format is
#'   defined by file ending eith html or docx).
#' @param type Character string. "df" for data-frame. "html" for html table.
#' @param ... Further arguments passed to the [cor.test()] function.
#'
#' @return A data-frame or a html table object
#' @examples
#' nice_corrmatrix(mtcars, ci = TRUE)
#' nice_corrmatrix(mtcars, 
#'   show_p = TRUE, 
#'   show_ci = TRUE, 
#'   show_stars = FALSE, 
#'   conf.level = 0.99
#' )
#' @export

nice_corrmatrix <- function(cr, 
                            upper = FALSE, 
                            lower = TRUE,
                            digits = 2, 
                            show_r = TRUE, 
                            show_p = FALSE,
                            show_stars = TRUE, 
                            show_ci = FALSE, 
                            numbered_columns = TRUE,
                            descriptives = TRUE,
                            labels = NULL,
                            nsig_p = .10, 
                            char_nsig, 
                            char_autocor = "\uFF0D",
                            char_p10 = "\u271e", 
                            char_NA = "", 
                            string_ci = "{break_sign}[{ci_lower},{ci_upper}]",
                            string_p = "{break_sign}(p {nice_p(p, equal_sign = TRUE)})",
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
    cr <- corrmatrix(cr, ...)
  }
  
  r <- cr$r
  p <- cr$p

  r <- round(r, digits)
  r <- format(r, digits = digits, nsmall = digits)

  if (!show_r) r[TRUE] <- ""

  if (!missing(char_nsig)) r[p > nsig_p] <- char_nsig
  r[is.na(cr$r)] <- char_NA
  diag(r) <- char_autocor
  diag(p) <- 1
  if (show_stars) {
    copy_r <- r
    r[which(p <= .10)] <- paste0(copy_r[which(p <= .10)], char_p10)
    r[which(p <= .05)] <- paste0(copy_r[which(p <= .05)], "*  ")
    r[which(p <= .01)] <- paste0(copy_r[which(p <= .01)], "** ")
    r[which(p <= .001)] <- paste0(copy_r[which(p <= .001)], "***")
    r[which(p > .10 & p < 1)] <- paste0(copy_r[which(p > .10 & p < 1)], "   ")
  }
  
  r[which(cr$r >= 0)] <- paste0(" ", r[which(cr$r >= 0)])
  
  if (show_ci) {
    new_env <- new.env()
    new_env$break_sign <- if (identical(type, "html")) "<br>" else " "
    new_env$ci_lower <- formatC(cr$ci$lower, format = 'f', digits = digits)
    new_env$ci_upper <- formatC(cr$ci$upper, format = 'f', digits = digits)
    new_env$r <- r
    #r[] <- paste0(r, break_sign, "[", lb, ", ", ub, "]")
    r[] <- glue::glue("{r} ", string_ci, .envir = new_env)
  }
  
  if (show_p) {
    new_env <- new.env()
    new_env$p <- cr$p
    new_env$r <- r
    new_env$break_sign <- if (identical(type, "html")) "<br>" else " "
    r[] <- glue::glue("{r} ", string_p, .envir = new_env)
  }
  
  diag(r) <- char_autocor
  
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
    if (show_stars) {
      note <- paste0(char_p10, "p<.10; *p<.05; **p<.01; ***p<.001.\n", sep = "")
      cat(note)
    }
    return(r)
  }

  if (type == "html") {
    out <- nice_table(
      r,
      file = file
    ) |> gt::fmt_markdown(columns = 5:ncol(r))
    return(out)
    
  }
}

corrmatrix <- function(x, digits = 2, p = TRUE, ci = FALSE, ...) {
  
  n_vars <- ncol(x)
  out_r <- matrix(rep(NA, n_vars * n_vars), ncol = n_vars)
  rownames(out_r) <- names(x)
  colnames(out_r) <- names(x)

  out_p <- out_t <- out_df <- out_upper <- out_lower <- out_r
  
  for(i in 1:n_vars) {
    for(j in 1:n_vars) {
      if(j == i) {
        out_r[i, j] <- 1
        next
      }  
      if (p) {
        res <- cor.test(x[[i]], x[[j]], ...)  
        out_r[i, j] <- res$estimate
        out_p[i, j] <- res$p.value
        out_t[i, j] <- res$statistic
        out_df[i, j] <- res$parameter
        out_lower[i, j] <- res$conf.int[1]
        out_upper[i, j] <- res$conf.int[2]
      } else {
        out_r[i, j] <- cor(x[[i]], x[[j]]) 
      }
      
    }
  }
  
  diag(out_p) <- 1
  list(
    r = out_r,
    p = out_p,
    df = out_df,
    ci = list(lower = out_lower, upper = out_upper),
    t = out_t
  )  
}

