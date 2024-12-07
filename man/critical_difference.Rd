% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/critical_difference.R
\name{critical_difference}
\alias{critical_difference}
\title{Compute the Critical Difference Using SEM and SED}
\usage{
critical_difference(rtt, sd, ci)
}
\arguments{
\item{rtt}{Numeric value between 0 and 1. The test-retest reliability
coefficient of the test.}

\item{sd}{Numeric value. The standard deviation of the test scores.}

\item{ci}{Numeric value between 0 and 1. The desired confidence interval
level (e.g., 0.95 for a 95\% confidence interval).}
}
\value{
A list containing:
\describe{
\item{\code{critical_difference}}{The critical difference value.}
\item{\code{z}}{The z-score corresponding to the specified confidence interval.}
\item{\code{standard_error_measurement}}{The standard error of measurement (SEM).}
\item{\code{standard_error_difference}}{The standard error of difference (SED).}
}
}
\description{
Calculates the critical difference required to determine whether the
difference between two test scores is statistically significant. This
function computes the critical difference based on the test-retest
reliability coefficient, the standard deviation of the test scores, and the
desired confidence interval. It also returns the z-score, standard error of
measurement (SEM), and standard error of difference (SED).
}
\details{
The function calculates the critical difference (CD) using the
following steps:
\itemize{
\item \strong{Calculate the z-score (\eqn{z}) corresponding to the desired confidence interval:}
\deqn{z = \text{qnorm}\left( \frac{1 - \text{ci}}{2}, \text{lower.tail} = FALSE \right)}
\item \strong{Compute the standard error of measurement (SEM):}
\deqn{\text{SEM} = \text{SD} \times \sqrt{1 - r_{tt}}}
\item \strong{Compute the standard error of difference (SED):}
\deqn{\text{SED} = \sqrt{2} \times \text{SEM}}
\item \strong{Calculate the critical difference (CD):}
\deqn{\text{CD} = z \times \text{SED}}
}
This approach accounts for the reliability of the test and the desired
level of confidence to determine if the difference between two scores is
statistically significant.
}
\examples{
# Calculate the critical difference for rtt = 0.9, sd = 15, ci = 0.95
critical_difference(rtt = 0.9, sd = 15, ci = 0.95)

}
