#' Convert Logit to Probability and vice versa
#'
#' This function takes a logit value and converts it to a probability using the logistic function and vice versa.
#'
#' @param logit Numeric. The logit value to be converted to probability.
#' @param prob Numeric. A probability to be converted to a logit.
#'
#' @return Numeric.
#'
#' @examples
#' logit2prob(0) # Returns 0.5
#' logit2prob(2) # Returns 0.8807971
#' prob2logit(0.8807971)
#' @export

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  prob
}


#' @rdname logit2prob
#' @export
prob2logit <- function(prob){
  logit <- log(prob / (1 - prob))
  logit
}
