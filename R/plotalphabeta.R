#' Plot Alpha and Beta Error Regions
#'
#' Generates a plot of two normal distributions with Type I (alpha) and Type II (beta)
#' error areas shaded. Legend and effect size annotation are embedded in the plot.
#'
#' @param m Mean under H0.
#' @param sd0 Standard deviation under H0.
#' @param sd1 Standard deviation under H1.
#' @param d Effect size (Cohen's d): H₁ mean is \code{m + d * sd0}.
#' @param alpha Significance level.
#' @param fill Logical. Whether to shade alpha and beta areas.
#' @param col_fill Character vector of length 2: colors for alpha and beta shading.
#' @param show_text Logical. If TRUE, prints α, β, and power in the plot.
#' @param show_labels Logical. If TRUE, adds labels for H0, H1, and effect size d.
#'
#' @return A ggplot object.
#' @examples
#' plotalphabeta(m = 0, sd0 = 1, sd1 = 1.5, d = 1.5, alpha = 0.01)
#' 
#' @export
plotalphabeta <- function(m = 0,
                          sd0 = 1,
                          sd1 = 1,
                          d = 2,
                          alpha = 0.05,
                          fill = TRUE,
                          col_fill = c("pink", "lightblue"),
                          show_text = TRUE,
                          show_labels = TRUE) {
  
  if (length(col_fill) != 2) stop("`col_fill` must be a character vector of length 2.")
  
  m1 <- m + d * sd0
  crit <- qnorm(1 - alpha, mean = m, sd = sd0)
  beta <- pnorm(crit, mean = m1, sd = sd1)
  power <- 1 - beta
  
  x_vals <- seq(min(m - 4 * sd0, m1 - 4 * sd1),
                max(m + 4 * sd0, m1 + 4 * sd1), length.out = 2000)
  
  df <- data.frame(
    x = x_vals,
    density_H0 = dnorm(x_vals, mean = m, sd = sd0),
    density_H1 = dnorm(x_vals, mean = m1, sd = sd1)
  )
  
  if (fill) {
    df_fill <- rbind(
      data.frame(x = df$x[df$x >= crit],
                 y = df$density_H0[df$x >= crit],
                 region = "Type I error (α)"),
      data.frame(x = df$x[df$x <= crit],
                 y = df$density_H1[df$x <= crit],
                 region = "Type II error (β)")
    )
  }
  
  p <- ggplot(df, aes(x = x)) +
    theme_minimal(base_size = 12) +
    labs(x = "x", y = "Density") +
    geom_line(aes(y = density_H0), color = "black", size = 0.7) +
    geom_line(aes(y = density_H1), color = "black", size = 0.7, linetype = "dashed") +
    geom_vline(xintercept = m, linetype = "dotted") +
    geom_vline(xintercept = m1, linetype = "dotted") +
    #geom_vline(xintercept = crit, linetype = "solid")
    geom_segment(
      aes(x = crit, xend = crit, y = 0, yend = max(
        dnorm(crit, mean = m, sd = sd0),
        dnorm(crit, mean = m + d * sd0, sd = sd1)
      )),
      linetype = "solid", color = "grey50"
    )
  
  if (fill) {
    p <- p +
      geom_area(data = df_fill,
                aes(y = y, fill = region),
                alpha = 0.5,
                position = "identity",
                show.legend = TRUE) +
      scale_fill_manual(values = setNames(col_fill, c("Type I error (α)", "Type II error (β)")),
                        name = NULL)
  }
  
  # Textbox oben rechts
  if (show_text) {
    label <- sprintf("α = %.3f\nβ = %.3f\nPower = %.3f", alpha, beta, power)
    xpos <- min(df$x) + 0.05 * diff(range(df$x))
    ypos <- max(df$density_H0, df$density_H1) * 0.95
    p <- p + annotate("text", x = xpos, y = ypos, label = label, hjust = 0, vjust = 1)
  }
  
  if (show_labels) {
    ymax <- max(df$density_H0, df$density_H1)
    # H0 and H1 labels
    p <- p +
      annotate("text", x = m, y = ymax * 0.85, label = "H0", hjust = 1.2) +
      annotate("text", x = m1, y = ymax * 0.85, label = "H1", hjust = -0.2)
    # Effect size arrow
    p <- p +
      annotate("segment", x = m, xend = m1, y = ymax * 0.75, yend = ymax * 0.75,
               arrow = arrow(ends = "both", length = unit(0.1, "inches")),
               color = "darkgray") +
      annotate("text", x = (m + m1) / 2, y = ymax * 0.78, label = "Effect size (d)", hjust = 0.5)
  }
  
  p <- p + theme(legend.position = c(0.95, 0.95),
                   legend.justification = c("right", "top"),
                   legend.background = element_rect(fill = "white", color = "gray80"))
  
  return(p)
}
