#' Plot ALpha / Beta graph
#' Generates a graphic for visualizing alpha and beta error
#' @param m Mean
#' @param sd Standard Deviation
#' @param d Effect size
#' @param alpha Alpha Level
#' @param fill If TRUE intersections are filled out in grey
#' @param col_fill Vector of length two with colornames to fill area below curve 
#' @export
plotalphabeta <- function(m = 0, sd = 1, d = 2, alpha = 0.05, fill = TRUE, col_fill = c("grey75", "grey50")) {
  crit <- qnorm(1 - alpha, 0, 1)
  x <- seq(-4 * sd + m, (4 + d) * sd + m, length = 1000)
  y <- dnorm(x, mean = m, sd = sd)
  z <- dnorm(x, mean = d * sd + m, sd = sd)
  plot(x, y, type = "n", ylab = "Density", xlab = "x")
  
  segments(
    m, dnorm(m, mean = m, sd = sd), m, 0, 
    col = "black", lwd = 0.8, lty = "dashed"
  )
  segments(
    m + sd * d, dnorm(m + sd * d, mean = m + sd * d, sd = sd), 
    m + sd * d, 0, 
    col = "black", lwd = 0.8, lty = "dashed"
  )
  
  if (fill) { 
    # alpha error
    x1 <- seq(m + crit * sd, m + (4 + d) * sd, length = 100)
    y1 <- dnorm(x1, mean = m, sd = sd)
    .fill(x1, y1, col_fill[1])
    
    # beta error
    x1 <- seq(m - 4 * sd, m + crit * sd, length = 200)
    y1 <- dnorm(x1, mean = d * sd + m, sd = sd)
    .fill(x1, y1, col_fill[2])
  }
  segments(
    m + crit * sd, 
    dnorm((m + d * sd) - crit * sd, mean = m, sd = sd), 
    m + crit * sd, 
    0, 
    col = "black", 
    lwd = 0.8, 
    lty = "solid"
  )
  lines(x, y, type = "l")
  lines(x, z, type = "l")
}

.fill <- function(x, y, col = "grey50") {
  for(i in 1:length(x)) {
    x_values <- c(x[i], x[i+1], x[i+1], x[i])
    y_values <- c(0, 0, y[i+1], y[i])
    polygon(x_values, y_values, col = col, border = NA)      
  }
}
