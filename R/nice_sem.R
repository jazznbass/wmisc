#' Nice html table for lavaan structure equation model fitted objects
#' 
#' @param x An object returned from the lavaan sem or cfa function.
#' @param remove_cols Either column number or column names to be removed. 
#' @param title Title string.
#' @param footnote/note Add footnote
#' @examples
#' nice_sem(wmisc::lavaan_fit)
#' @export
nice_sem <- function(x, 
                     standardized = TRUE, 
                     fitmeasures = c(
                       CFI = "cfi", TLI = "tli", RMSEA = "rmsea", 
                       "RMSEA ci lower" = "rmsea.ci.lower",
                       AIC = "aic", BIC = "bic"),
                     remove_cols = NULL,
                     round = 2,
                     title = "Structure equation model",
                     footnote = NULL) {

  if (standardized) out <- lavaan::standardizedsolution(x) |> as.data.frame()
  if (!standardized) out <- lavaan::parameterestimates(x) |> as.data.frame()
  
  rows_latents <- which(out$op == "=~")
  rows_regressions <- which(out$op == "~")
  rows_variances <- which(out$op == "~~" & out$lhs == out$rhs)
  rows_covariances <- which(out$op == "~~" & out$lhs != out$rhs)
  rows_interecepts <-  which(out$op == "~1")
  
  out[rows_latents, 1] <- paste0(out[rows_latents, 1], " \u2192 ", out[rows_latents, 3] )
  out[rows_regressions, 1] <- paste0(out[rows_regressions, 1], " \u2190 ", out[rows_regressions, 3] )
  out[rows_covariances, 1] <- paste0(out[rows_covariances, 1], " \u2194 ", out[rows_covariances, 3] )
  
  
  row_group <- list(
    "Latent variables" = rows_latents,
    "Regressions" = rows_regressions,
    "Covariances" = rows_covariances,
    "Variances" = rows_variances,
    "Intercepts" = rows_interecepts
  )
  row_group <- row_group[sapply(row_group, \(.) length(.) > 0)]
  
  out <- out[, -c(2:3)]
  names(out) <- c("Parameter", "b", "se", "z", "p", "lower", "upper")
  out$p <- nice_p(out$p)
  out <- out[,c("Parameter", "b", "lower", "upper", "se", "z", "p")]
  
  # remove cols ----
  
  remove_ci <- FALSE
  if (!is.null(remove_cols)) {
    if (is.character(remove_cols)) {
      if ("ci" %in% remove_cols) remove_ci <- TRUE
      remove_cols <- which(names(out) %in% remove_cols)
      if (remove_ci) remove_cols <- c(3:4, remove_cols)
    }
    out <- out[,-remove_cols]
  }
  
  
  
  # add fit measures ----
  
  fit_first <- fitmeasures(x)
  
  out[nrow(out)+1, 1] <- "Parameters"
  out[nrow(out), 2] <- fit_first["npar"]
  out[nrow(out)+1, 1] <- "Observations"
  out[nrow(out), 2] <- fit_first["ntotal"]
  
  
  out[nrow(out)+1, 1] <- paste0("\u03a7\u00b2(", fit_first["df"], ")")
  out[nrow(out), 2] <- fit_first["chisq"]
  out[nrow(out)+1, 1] <- "p"
  out[nrow(out), 2] <- fit_first["pvalue"]
  
  fit <- fitmeasures(x, fitmeasures)
  
  rows <- (nrow(out)+1):(nrow(out)+length(fit))
  out[rows, 1] <- names(fitmeasures)
  out[rows, 2] <- fit

  row_group$"Modelfit" <- (nrow(out) - length(fit) - 3):nrow(out)
  
  out <- round_numeric(out, round)
  
  if (is.null(footnote)) {
    if(standardized) footnote <- "Effect estimates are standardized"
    if(!standardized) footnote <- "Effect estimates are not standardized"
  }
  
  out <- set_wmisc_attributes(
    out,
    title = title,
    row_group = row_group,
    footnote = footnote,
    label_na = ""
  )
  
  if (!remove_ci) out <- set_wmisc_attributes(
    out,spanner = list("95% CI" = 3:4))

  nice_table(out)
  
}


