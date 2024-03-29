% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nice_chi_test_table.R
\name{nice_chi_test_table}
\alias{nice_chi_test_table}
\alias{chi_test_table}
\title{Creates a chi.squared test table for multiple dependent variables}
\usage{
nice_chi_test_table(
  dv,
  iv,
  data,
  conditions = levels(factor(iv))[1:2],
  ref_levels = 2,
  labels = NULL,
  nice_p = TRUE,
  digits = 2,
  order = "12",
  label_attr = TRUE,
  type = "html",
  ...
)

chi_test_table(...)
}
\arguments{
\item{dv}{A data frame with the dependent variables or a character vector
when argument data is set.}

\item{iv}{A data frame or vector with the independent variable or a character
if argument data is set.}

\item{data}{A data frame.}

\item{conditions}{A character vector of length two with the names of the two
conditions. Defaults to the first two levels of the independent variable
'iv' if applicable.}

\item{ref_levels}{Vector with the reference level for each dependent
variable. Values are repeated if length of levels vector is smaller than
number dependent variables.}

\item{labels}{A character vector of length two with labels for the dependent
variables.}

\item{nice_p}{If TRUE, p values are printed in a nice format.}

\item{digits}{Number of digits for rounding mean and sd values}

\item{order}{Either "12" or "21" depicting whether group two is compared to
group one or vice versa.}

\item{type}{Either "df" for data frame or "html" for html table.}

\item{...}{Further arguments passed to \code{\link[=nice_table]{nice_table()}}}
}
\value{
A tibble or an html table
}
\description{
Creates a chi.squared test table for multiple dependent variables
}
\examples{
dat <- data.frame(
  gender = factor(rbinom(100, 1, 0.3), labels = c("male", "female")),
  glasses = factor(rbinom(100, 1, 0.3), labels = c("no", "yes")),
  sen = factor(rbinom(100, 1, 0.5), labels = c("no_sen", "sen"))
 )
chi_test_table(
  c("gender", "glasses"), "sen", data = dat, 
  ref_levels = c("male", "no")
)
}
