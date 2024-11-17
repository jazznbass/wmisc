#' Compute the Critical Difference Using SEM and SED
#'
#' Calculates the critical difference required to determine whether the
#' difference between two test scores is statistically significant. This
#' function computes the critical difference based on the test-retest
#' reliability coefficient, the standard deviation of the test scores, and the
#' desired confidence interval. It also returns the z-score, standard error of
#' measurement (SEM), and standard error of difference (SED).
#'
#' @param rtt Numeric value between 0 and 1. The test-retest reliability
#'   coefficient of the test.
#' @param sd Numeric value. The standard deviation of the test scores.
#' @param ci Numeric value between 0 and 1. The desired confidence interval
#'   level (e.g., 0.95 for a 95% confidence interval).
#'
#' @details The function calculates the critical difference (CD) using the
#'   following steps:
#' \itemize{
#'   \item **Calculate the z-score (\eqn{z}) corresponding to the desired confidence interval:**
#'     \deqn{z = \text{qnorm}\left( \frac{1 - \text{ci}}{2}, \text{lower.tail} = FALSE \right)}
#'   \item **Compute the standard error of measurement (SEM):**
#'     \deqn{\text{SEM} = \text{SD} \times \sqrt{1 - r_{tt}}}
#'   \item **Compute the standard error of difference (SED):**
#'     \deqn{\text{SED} = \sqrt{2} \times \text{SEM}}
#'   \item **Calculate the critical difference (CD):**
#'     \deqn{\text{CD} = z \times \text{SED}}
#' }
#'   This approach accounts for the reliability of the test and the desired
#'   level of confidence to determine if the difference between two scores is
#'   statistically significant.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{critical_difference}}{The critical difference value.}
#'   \item{\code{z}}{The z-score corresponding to the specified confidence interval.}
#'   \item{\code{standard_error_measurement}}{The standard error of measurement (SEM).}
#'   \item{\code{standard_error_difference}}{The standard error of difference (SED).}
#' }
#'
#' @examples
#' # Calculate the critical difference for rtt = 0.9, sd = 15, ci = 0.95
#' critical_difference(rtt = 0.9, sd = 15, ci = 0.95)
#'
#' @importFrom stats qnorm
#' @export
critical_difference <- function(rtt, sd, ci) {
  z <- qnorm((1 + ci) / 2)
  sem <- sd * sqrt(1 - rtt)
  sed <- sqrt(2) * sem
  cd <- z * sed
  list(
    critical_difference = cd,
    z = z,
    standard_error_meassurement = sem,
    standard_error_difference = sed
  )
}

