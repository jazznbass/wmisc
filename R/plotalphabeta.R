#' Plot ALpha / Beta graph
#' enerates a graphic for visualizing alpha and beta error
#' @param m Mean
#' @param sd Standard Deviation
#' @param d Effect size
#' @param alpha Alpha Level
#' @param svg.out If set a svg file with the given name is created
#' @param fill If TRUE intersections are filled out in grey
#'
#' @return
#' @export
#'
#' @examples
plotalphabeta <- function(m = 0, sd = 1, d = 3, alpha = 0.01, svg.out = "", fill = TRUE) {
  crit <- qnorm(1 - alpha, 0, 1)
  x <- seq(-4 * sd + m, (4 + d) * sd + m, length = 1000)
  y <- dnorm(x, mean = m, sd = sd)
  z <- dnorm(x, mean = d * sd + m, sd = sd)
  if (svg.out != "") svg(svg.out, width = 7, height = 7)
  plot(x, y, type = "n")
  
  segments(m, dnorm(m, mean = m, sd = sd), m, 0, col = "black", lwd = 0.8, lty = "dashed")
  segments(m + sd * d, dnorm(m + sd * d, mean = m + sd * d, sd = sd), m + sd * d, 0, col = "black", lwd = 0.8, lty = "dashed")
  
  if (fill) {
    # alpha error
    x1 <- seq(m + crit * sd, m + (4 + d) * sd, length = 100)
    y1 <- dnorm(x1, mean = m, sd = sd)
    x_fill(x1, y1)
    
    # beta error
    x1 <- seq(m - 4 * sd, m + crit * sd, length = 200)
    y1 <- dnorm(x1, mean = d * sd + m, sd = sd)
    x_fill(x1, y1)
  }
  segments(m + crit * sd, dnorm((m + d * sd) - crit * sd, mean = m, sd = sd), m + crit * sd, 0, col = "black", lwd = 0.8, lty = "solid")
  lines(x, y, type = "l")
  lines(x, z, type = "l")
  if (svg.out != "") dev.off()
}
