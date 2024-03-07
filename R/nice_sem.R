#' Nice html table for lavaan structure equation model fitted objects
#' 
#' @param x An object returned from the lavaan sem or cfa function.
#' @param remove_cols Either column number or column names to be removed. 
#' @param show_fitmeasures A named vector with fit measures.
#' @param show_ci If TRUE, adds columns with 95% confidence intervals.
#' @param ... Further arguments passed to the `nice_table()` function.
#' @examples
#' nice_sem(wmisc::lavaan_fit)
#' @export
nice_sem <- function(x, 
                     standardized = TRUE, 
                     show_fitmeasures = c(
                       CFI = "cfi", TLI = "tli", RMSEA = "rmsea", 
                       "RMSEA ci lower" = "rmsea.ci.lower",
                       "SRMR" = "srmr", 
                       AIC = "aic", BIC = "bic"),
                     remove_cols = NULL,
                     show_ci = TRUE,
                     round = 2,
                     ...) {

  
  args_nice_table <- list(...)
  
  if (standardized) {
    out <- lavaan::standardizedsolution(x) |> as.data.frame()
    char_estimate <- "ß"
  } else {
    out <- lavaan::parameterestimates(x) |> as.data.frame()
    char_estimate <- "b"
  }
  
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
  names(out) <- c("parameter", char_estimate, "se", "z", "p", "lower", "upper")
  out$p <- nice_p(out$p)
  out <- out[,c("parameter", char_estimate, "lower", "upper", "se", "z", "p")]
  
  # remove cols ----
  
  if (!show_ci) out <- out[,-(3:4)]
  if (!is.null(remove_cols)) {
    if (is.character(remove_cols)) {
      remove_cols <- which(names(out) %in% remove_cols)
    }
    out <- out[,-remove_cols]
  }
  
  # add fit measures ----
  
  fit_first <- lavaan::fitmeasures(x)
  
  out[nrow(out)+1, 1] <- "N parameters"
  out[nrow(out), 2] <- fit_first["npar"]
  out[nrow(out)+1, 1] <- "Observations"
  out[nrow(out), 2] <- fit_first["ntotal"]
  
  
  out[nrow(out)+1, 1] <- paste0("\u03a7\u00b2(", fit_first["df"], ") / p-value")
  out[nrow(out), 2] <- fit_first["chisq"]
  #out[nrow(out)+1, 1] <- "p"
  out[nrow(out), 3] <- fit_first["pvalue"]
  
  fit <- lavaan::fitmeasures(x, show_fitmeasures)
  
  rows <- (nrow(out)+1):(nrow(out)+length(fit))
  out[rows, 1] <- names(show_fitmeasures)
  out[rows, 2] <- fit

  row_group$"Modelfit" <- (nrow(out) - length(fit) - 3):nrow(out)
  
  out <- round_numeric(out, round)
  
  if (is.null(footnote)) {
    if(standardized) footnote <- "Effect estimates are standardized"
    if(!standardized) footnote <- "Effect estimates are not standardized"
  }
  
  if (is.null(args_nice_table$title)) args_nice_table$title <- "Structure equation model"

  if (is.null(args_nice_table$footnote)) {
    cit <- unclass(citation("lavaan"))[[1]]
    args_nice_table$footnote <- c(
      
      if (x@Options$estimator == "ML") {
        "The estimation method employed in this analysis is Maximum Likelihood (ML)"
      } else paste0("The estimation method employed in this analysis is ", x@Options$estimator),  
      if (x@Options$optim.method == "nlminb") {
       "The Non-Linear Minimization, Bounded (nlminb) algorithm was applied for optimization"
      } else paste0("Optimization method is ", x@Options$optim.method),
      if (identical(x@Options$missing, "ml")) 
        "Missing data were addressed using the Full Information Maximum Likelihood (FIML) approach",
      paste0("The analysis was performed with the lavaan package in R (",
              cit$author, ", ", cit$year, ")")
      
      
    )
  }
  
  out <- set_wmisc_attributes(
    out,
    row_group = row_group,
    label_na = ""
  )
  
  if (show_ci) out <- set_wmisc_attributes(
    out, spanner = list("95% CI" = 3:4))
  
  args_nice_table$x <- out

  do.call(nice_table, args_nice_table)

}


