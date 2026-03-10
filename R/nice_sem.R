#' Nice html table for lavaan structure equation model fitted objects
#' 
#' This function takes an object returned from the `lavaan` package's `sem` or `cfa` functions and produces a nicely formatted HTML table summarizing the model parameters and fit measures. It allows customization of displayed columns, inclusion of confidence intervals, and choice between standardized and unstandardized estimates.
#' 
#' @details
#' The function organizes the output into sections for latent variables, regressions, covariances
#' , variances, and intercepts. It also appends key fit measures at the end of the table.
#' Users can specify which columns to remove, whether to show confidence intervals, and whether to display standardized estimates.
#' It leverages the `nice_table()` function for final formatting, allowing further customization through additional arguments.
#' 
#' The resulting table includes a title and footnote that provide context about the estimation method, optimization algorithm, handling of missing data, and citation information for the `lavaan` package.
#' 
#' @return An HTML table summarizing the structure equation model.
#' @author Juergen Wilbert
#' @param x An object returned from the lavaan sem or cfa function.
#' @param remove_cols Either column number or column names to be removed. 
#' @param show_fitmeasures A named vector with fit measures.
#' @param show_ci If TRUE, adds columns with 95% confidence intervals.
#' @param standardized If TRUE, standardized estimates are shown.
#' @param round Number of digits to round numeric values.
#' @param ... Further arguments passed to the `nice_table()` function.
#' @examples
#' nice_sem(lavaan_fit)
#' nice_sem(lavaan_fit, standardized = FALSE, show_ci = FALSE)
#' nice_sem(
#'  lavaan_fit,
#'  show_fitmeasures = c(
#'   FI = "cfi", TLI = "tli", RMSEA = "rmsea",
#'   "SRMR" = "srmr"
#'  ),
#'  remove_cols = c("se", "z")
#' )
#'  
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
    char_estimate <- "Beta"
  } else {
    out <- lavaan::parameterestimates(x) |> as.data.frame()
    char_estimate <- "b"
  }
  
  rows_latents <- which(out$op == "=~")
  rows_regressions <- which(out$op == "~")
  rows_variances <- which(out$op == "~~" & out$lhs == out$rhs)
  rows_covariances <- which(out$op == "~~" & out$lhs != out$rhs)
  rows_interecepts <-  which(out$op == "~1")
  rows_defined <- which(out$op == ":=")
  
  out[rows_latents, 1] <- paste0(out[["lhs"]][rows_latents], " \u2192 ", out[["rhs"]][rows_latents])
  out[rows_regressions, 1] <- paste0(out[["lhs"]][rows_regressions], " \u2190 ", out[["rhs"]][rows_regressions])
  out[rows_covariances, 1] <- paste0(out[["lhs"]][rows_covariances], " \u2194 ", out[["rhs"]][rows_covariances])
  out[rows_defined, 1] <- paste0(out[["rhs"]][rows_defined])
  
  if ("label" %in% names(out)) {
    id <- which(out[["label"]] != "")
    out[[1]][id] <- paste0(out[[1]][id], " (", out[["label"]][id], ")")
    out <- out[, setdiff(names(out), "label")]
  }
  
  
  row_group <- list(
    "Latent variables" = rows_latents,
    "Regressions" = rows_regressions,
    "Covariances" = rows_covariances,
    "Variances" = rows_variances,
    "Intercepts" = rows_interecepts,
    "Defined" = rows_defined
  )
  row_group <- row_group[sapply(row_group, \(.) length(.) > 0)]
  
  nm <- names(out)
  names(out)[which(nm == "lhs")] <- "parameter"
  names(out)[which(nm == "est.std")] <- char_estimate
  names(out)[which(nm == "pvalue")] <- "p"
  names(out)[which(nm == "ci.lower")] <- "lower"
  names(out)[which(nm == "ci.upper")] <- "upper"
  
  out <- out[, setdiff(names(out), c("op", "rhs"))]
  out$p <- nice_p(out$p)

  # remove cols ----
  
  if (!show_ci) {
    out <- out[, setdiff(names(out), c("lower", "upper"))]
  }
  if (!is.null(remove_cols)) {
    if (is.character(remove_cols)) {
      remove_cols <- which(names(out) %in% remove_cols)
    }
    out <- out[,-remove_cols]
  }
  
  # add fit measures ----
  
  fit_first <- lavaan::fitmeasures(x)
  
  out[nrow(out) + 1, 1] <- "N parameters"
  out[nrow(out), 2] <- fit_first["npar"]
  out[nrow(out) + 1, 1] <- "Observations"
  out[nrow(out), 2] <- fit_first["ntotal"]
  
  
  out[nrow(out) + 1, 1] <- paste0("\u03a7\u00b2(", fit_first["df"], ") / p-value")
  out[nrow(out), 2] <- fit_first["chisq"]
  #out[nrow(out)+1, 1] <- "p"
  out[nrow(out), 3] <- fit_first["pvalue"]
  
  fit <- lavaan::fitmeasures(x, show_fitmeasures)
  
  rows <- (nrow(out)+1):(nrow(out)+length(fit))
  out[rows, 1] <- names(show_fitmeasures)
  out[rows, 2] <- fit

  row_group$"Modelfit" <- (nrow(out) - length(fit) - 3):nrow(out)
  
  out <- round_numeric(out, round)
  
  #if (is.null(footnote)) {
  #  if(standardized) footnote <- "Effect estimates are standardized"
  #  if(!standardized) footnote <- "Effect estimates are not standardized"
  #}
  
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
              paste0(cit$author, collapse = ", "), ", ", cit$year, ")")
      
      
    )
  }
  
  out <- set_wmisc_attributes(
    out,
    row_group = row_group,
    label_na = ""
  )
  
  if (show_ci) {
    cols <- which(names(out) %in% c("lower", "upper"))
    out <- set_wmisc_attributes(out, 
      spanner = list("95% CI" = cols[1]:cols[2]))
  }
  
  args_nice_table$x <- out

  do.call(nice_table, args_nice_table)

}


