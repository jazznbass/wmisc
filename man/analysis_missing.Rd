% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis_missing.R
\name{analysis_missing}
\alias{analysis_missing}
\title{Analyse Missing Values in Scales}
\usage{
analysis_missing(dat, scale)
}
\arguments{
\item{dat}{A data frame containing the dataset to be analyzed.}

\item{scale}{A list of character vectors, where each list element contains the
variable names corresponding to a scale within the dataset.}
}
\value{
A data frame summarizing the missing value analysis for each scale. The output includes:
\itemize{
\item \code{missing}: The total number of missing values for each scale.
\item \code{total}: The total number of observations expected for each scale.
\item \code{p}: The proportion of missing values for each scale (\code{missing / total}).
\item \verb{n cases}: The number of cases with at least one missing value within the scale.
\item \verb{p cases}: The proportion of cases with at least one missing value (\verb{n cases / total cases}).
\item \verb{n all cases}: The number of cases where all variables in the scale are missing.
\item \verb{p all cases}: The proportion of cases where all variables in the scale are missing (\verb{n all cases / total cases}).
}
}
\description{
This function analyzes missing values for specified scales within a data frame.
}
\examples{
# Example dataset
dat <- data.frame(
  scale1_var1 = c(1, 2, NA, 4),
  scale1_var2 = c(NA, 2, 3, 4),
  scale2_var1 = c(1, NA, 3, 4),
  scale2_var2 = c(NA, NA, NA, 4)
)

# Define scales
scales <- list(
  scale1 = c("scale1_var1", "scale1_var2"),
  scale2 = c("scale2_var1", "scale2_var2")
)

# Analyze missing values
analysis_missing(dat, scales)

}
\seealso{
\code{\link[=is.na]{is.na()}}, \code{\link[=apply]{apply()}}
}
