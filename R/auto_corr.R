#' Calculate Autocorrelation
#'
#' This function calculates the autocorrelation for a given time series data. It
#' estimates the autocorrelation coefficients, standard errors, test statistics,
#' and p-values for each lag up to a specified maximum lag.
#'
#' @param x A numeric vector representing the time series data.
#' @param lag The maximum lag for which autocorrelation is to be calculated.
#' @param alpha The significance level for hypothesis tests. Default is 0.05.
#'
#' @return A data frame containing the lag, autocorrelation coefficients (ar),
#'   standard errors (se), test statistics (statistic), p-values (p_z and p_t),
#'   overall standard errors (se_all), Ljung-Box test statistic (q_lb), and
#'   p-value for the Ljung-Box test (p_all).
#'
#' @references Bartlett, M. S. (1946). On the theoretical specification and
#' sampling properties of autocorrelated time-series. \emph{Supplement to the
#' Journal of the Royal Statistical Society}, 8(1), 27-41.
#'
#' Ljung, G. M., & Box, G. E. P. (1978). On a measure of lack of fit in time
#' series models. \emph{Biometrika}, 65(2), 297-303.
#'
#' @seealso \url{https://de.wikipedia.org/wiki/Korrelogramm}
#' \url{http://sfb649.wiwi.hu-berlin.de/fedc_homepage/xplore/tutorials/xegbohtmlnode39.html}
#' \url{https://en.wikipedia.org/wiki/Ljung%E2%80%93Box_test}
#'
#' @examples
#' data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#' auto_corr(data, lag = 3)
auto_corr <- function(x, lag, alpha = 0.05) {
  m_x <- mean(x)
  t <- length(x)
  
  ar <- numeric(lag)
  for(l in 1:lag) {
    ar[l] <- sum((x[(1 + l):t])  * (x[1:(t - l)] - m_x)) / sum((x - m_x)^2)    
  }
  
  se <- numeric(lag)
  
  # <https://de.wikipedia.org/wiki/Korrelogramm>
  # Bartletts Formel für MA(l)
  for(l in 1:lag) se[l] <- sqrt((1 + 2 * sum(ar[1 - l]^2)) / (t))
  z <- ar / se
  
  p <- pnorm(abs(z), lower.tail = FALSE) * 2
  p_t <- pt(abs(z), df = 2, lower.tail = FALSE) * 2
  
  # <http://sfb649.wiwi.hu-berlin.de/fedc_homepage/xplore/tutorials/xegbohtmlnode39.html>
  se_all <- qnorm(1-(alpha / 2)) / sqrt(t)
  
  # <https://en.wikipedia.org/wiki/Ljung%E2%80%93Box_test>
  q_lb <- t * (t + 2) *  sum((ar^2)/(t - (1:lag)))
  p_all <- pchisq(q_lb, df = lag, lower.tail = FALSE)
  
  # return
  data.frame(
    lag = 1:lag, ar = ar, se = se, statistic = z, p_z = p, p_t, se_all = se_all, 
    q_lb = q_lb, p_all = p_all)
}
