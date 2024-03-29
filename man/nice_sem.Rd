% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nice_sem.R
\name{nice_sem}
\alias{nice_sem}
\title{Nice html table for lavaan structure equation model fitted objects}
\usage{
nice_sem(
  x,
  standardized = TRUE,
  show_fitmeasures = c(CFI = "cfi", TLI = "tli", RMSEA = "rmsea", `RMSEA ci lower` =
    "rmsea.ci.lower", SRMR = "srmr", AIC = "aic", BIC = "bic"),
  remove_cols = NULL,
  show_ci = TRUE,
  round = 2,
  ...
)
}
\arguments{
\item{x}{An object returned from the lavaan sem or cfa function.}

\item{show_fitmeasures}{A named vector with fit measures.}

\item{remove_cols}{Either column number or column names to be removed.}

\item{show_ci}{If TRUE, adds columns with 95\% confidence intervals.}

\item{...}{Further arguments passed to the \code{nice_table()} function.}
}
\description{
Nice html table for lavaan structure equation model fitted objects
}
\examples{
nice_sem(wmisc::lavaan_fit)
}
