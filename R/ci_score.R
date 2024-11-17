#' Compute Confidence Intervals for Test Scores Adjusted for Reliability
#'
#' @description Calculates the confidence interval around an observed test
#' score, adjusting for test reliability. Returns the confidence interval in raw
#' score units, z-scores, and percentiles.
#'
#' @param x Numeric value representing the observed test score.
#' @param rtt Numeric value between 0 and 1 indicating the reliability
#'   coefficient of the test (e.g., test-retest reliability).
#' @param m Numeric value representing the mean of the test scores in the
#'   population.
#' @param s Numeric value representing the standard deviation of the test scores
#'   in the population.
#' @param ci Numeric value between 0 and 1 indicating the desired confidence
#'   level (e.g., 0.95 for a 95% confidence interval).
#'
#' @details The standard error (\code{se}) is calculated using the formula:
#' \deqn{se = s \times \sqrt{1 - rtt}} where \code{s} is the standard deviation
#' and \code{rtt} is the reliability coefficient.
#'
#' The confidence interval is computed using the normal distribution quantile
#' corresponding to the specified confidence level.
#'
#' @return A list containing the following components:
#' \describe{
#'   \item{\code{scale}}{
#'     A named numeric vector with the confidence interval in raw score units:
#'     \itemize{
#'       \item \code{lower}: Lower bound of the confidence interval.
#'       \item \code{x}: The observed test score.
#'       \item \code{upper}: Upper bound of the confidence interval.
#'       \item \code{range}: Width of the confidence interval.
#'     }
#'   }
#'   \item{\code{z}}{
#'     A named numeric vector with the confidence interval in z-score units:
#'     \itemize{
#'       \item \code{lower}: Lower bound in z-scores.
#'       \item \code{x}: Observed score in z-scores.
#'       \item \code{upper}: Upper bound in z-scores.
#'       \item \code{range}: Width of the confidence interval in z-scores.
#'     }
#'   }
#'   \item{\code{percentile}}{
#'     A named numeric vector with the confidence interval in percentiles:
#'     \itemize{
#'       \item \code{lower}: Lower percentile.
#'       \item \code{x}: Percentile rank of the observed score.
#'       \item \code{upper}: Upper percentile.
#'       \item \code{range}: Width of the confidence interval in percentiles.
#'     }
#'   }
#'   \item{\code{parameters}}{
#'     A named numeric vector with additional parameters:
#'     \itemize{
#'       \item \code{se}: Standard error of measurement adjusted for reliability.
#'       \item \code{ci}: Confidence level used.
#'       \item \code{z}: Z-score multiplier corresponding to the confidence level.
#'     }
#'   }
#' }
#'
#' @examples
#' # Calculate a 95% confidence interval for an observed score of 100
#' ci_score(x = 100, rtt = 0.9, m = 100, s = 15, ci = 0.95)
#'
#' @export
ci_score <- function(x, rtt, m, s, ci) {
  se <- s * sqrt(1 - rtt)
  z <- qnorm((1 + ci) / 2)
  
  out <- list()
  out$scale <- c(
    lower = x - z * se, x = x,
    upper = x + z * se,
    range = (x + z * se) - (x - z * se)
  ) |> round(2)
  out$z = c(
    lower = ((x - z * se) - m) / s,
    x = (x - m) / s,
    upper = ((x + z * se) - m) / s,
    range = (z*se)/s*2
  ) |> round(3)
  out$percentile <- pnorm(out$z[1:3]) * 100
  out$percentile[4] <- out$percentile[3]- out$percentile[1]
  out$percentile <- setNames(
    out$percentile, c("lower", "x", "upper", "range")
  ) |> round(1)
  out$parameters <- c(
    se = se,
    ci = ci,
    z = z
  ) |> round(3)
  out
}


