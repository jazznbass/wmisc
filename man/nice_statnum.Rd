% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nice_statnum.R
\name{nice_statnum}
\alias{nice_statnum}
\title{Nice statnum
Formats numbers in the -1 to 1 range}
\usage{
nice_statnum(x, digits = 2)
}
\arguments{
\item{x}{number}

\item{digits}{Number to round to}
}
\description{
Nice statnum
Formats numbers in the -1 to 1 range
}
\examples{
nice_statnum(c(1, -1, runif(4, -1, 1)))

}
