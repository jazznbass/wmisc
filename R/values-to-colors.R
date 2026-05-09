#' Map continuous values to diverging colors (dark red -> white -> dark green)
#'
#' @param labels Character vector of node labels (names to use in the result).
#' @param x Numeric vector. Can be named or unnamed, but must align
#'   with `labels` by name (preferred) or by position (same length).
#' @param min Minimum value that maps to the darkest red (values below are
#'  clipped).
#' @param max Maximum value that maps to the darkest green (values above are 
#'  clipped).
#' @param low Color for low end (default dark red).
#' @param mid Color for midpoint (default white).
#' @param high Color for high end (default dark green).
#' @param n Number of discrete colors used internally for the gradient.
#'
#' @return Named character vector of hex colors with names equal to `labels`.
#' @export
#' @examples
#' z_values <- c(-2.5, -1, 0, 1, 2.5)
#' names(z_values) <- paste0("Node", 1:5)
#' colors <- values_to_colors_continuous(z_values)
#' print(colors)
#' t_values <- z_values * 10 + 50
#' names(t_values) <- paste0("Node", 1:5)
#' colors <- values_to_colors_continuous(t_values, min = 20, max = 80, neutral_min = 45, neutral_max = 55)
#' print(colors)
values_to_colors_continuous <- function(x,
                       labels = NULL,
                       min = -3,
                       max =  3,
                       neutral_min = -0.5,
                       neutral_max = 0.5,
                       low = "#7f0000",
                       mid = "#ffffff",
                       high = "#006400",
                       n = 201) {

  if (is.null(labels)) labels <- names(x)
  
  ### clip z values
  x <- pmax(pmin(x, max), min)
  
  # Palettes for each side (exclude pure white)
  red_to_white   <- grDevices::colorRampPalette(c(low,  mid))(n + 1)[1:n]
  white_to_green <- grDevices::colorRampPalette(c(mid, high))(n + 1)[2:(n + 1)]
  
  out <- character(length(x))
  
  # White dead-zone
  in_white <- (x >= neutral_min) & (x <= neutral_max)
  out[in_white] <- mid
  
  # Negative side: map [min .. -eps] -> [low .. almost-white]
  neg <- x < neutral_min
  if (any(neg)) {
    t <- (x[neg] - min) / ((neutral_min) - min)         # 0..1
    idx <- 1 + floor(t * (n - 1))                     # 1..n
    out[neg] <- red_to_white[idx]
  }
  
  # Positive side: map [eps .. z_max] -> [almost-white .. high]
  pos <- x > neutral_max
  if (any(pos)) {
    t <- (x[pos] - neutral_max) / (max - neutral_max)              # 0..1
    idx <- 1 + floor(t * (n - 1))                     # 1..n
    out[pos] <- white_to_green[idx]
  }
  
  #####
  
  names(out) <- labels
  out
}


#' Map discrete values to colors.
#' 
#' @param x Numeric or factor vector. Can be named or unnamed, but must align
#'    with `values` by name (preferred) or by position (same length).
#' @param values Numeric or factor vector of values to map (same type as `x`).
#' @param colors Character vector of hex color codes (same length as `values`
#'  and in the same order).
#' @return Named character vector of hex colors with names equal to `x`.
#' @export
#' @examples
#'  x <- c("A", "B", "C", "A", "B")
#'  values <- c("A", "B", "C")
#'  colors <- c("#7f0000", "#ffffff", "#006400")
#'  result <- values_to_colors_discrete(x, values, colors)
#'  print(result)
values_to_colors_discrete <- function(x,
                             values,
                             colors) {
  
  out <- x
  
  for(i in seq_along(values)) {
    id <- which(x == values[i])
    if (length(id) > 0) out[id] <- colors[i] 
  }
  
  out
}


#' Plot a continuous color legend for the diverging color scheme.
#' 
#' @param min Minimum value that maps to the darkest red (values below are
#'  clipped).
#' @param max Maximum value that maps to the darkest green (values above are
#'  clipped).
#' @param neutral_min Minimum value of the white "dead zone" (values above are
#'  colored white).
#' @param neutral_max Maximum value of the white "dead zone" (values below are
#'  colored white).
#' @param low Color for low end (default dark red).
#' @param mid Color for midpoint (default white).
#' @param high Color for high end (default dark green).
#' @param n Number of discrete colors used internally for the gradient.
#' @param title Title for the legend.
#' @return A plot of the color legend.
#' @export
plot_continuous_legend <- function(min = -3,
                                   max =  3,
                                   neutral_min = -0.5,
                                   neutral_max = 0.5,                          
                                   low = "#7f0000",
                                   mid = "#ffffff",
                                   high = "#006400",
                                   n = 801,
                                   unit = 1,
                                   title = "Z") {
  
  
  seq <- seq(min, max, length.out = n)
  
  cols <- values_to_colors_continuous(
    labels = as.character(seq), # dummy labels
    x = seq,
    min = min,
    max = max,
    neutral_min = neutral_min,
    neutral_max = neutral_max,
    low = low,
    mid = mid,
    high = high
  )
  cols <- unname(cols)
  df <- data.frame(x = seq, y = max-min, fill = cols)
  
  ggplot(df, aes(x = x, y = y, fill = fill)) +
    geom_tile(na.rm = TRUE) +
    scale_fill_identity() +
    xlab("") +
    ylab("") +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(legend.position = "none") +
    labs(title = title) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    #coord_fixed(ratio = 0.2) +
    scale_x_continuous(breaks = seq(min, max, by = unit), limits = c(min, max))
  
}

#' Plot a discrete color legend for the discrete color scheme.
#' 
#' @param values Numeric or factor vector of values to map (same type as `x`).
#' @param colors Character vector of hex color codes (same length as `values`
#' and in the same order).
#' @param title Title for the legend.
#' @return A plot of the color legend.
#' @export
plot_discrete_legend <- function(values,
                                 colors,
                                 title = "Values") {
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  par(mar = c(3.5, 2.5, 2, 2))  # short & wide
 
  # ggplot version
  
  df <- data.frame(x = as.character(values), y = 1, fill = colors)
  ggplot(df, aes(x = x, y = y, fill = fill)) +
    geom_tile() +
    scale_fill_identity() +
    theme_void() +
    theme(legend.position = "none") +
    labs(title = title) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    coord_fixed(ratio = 0.2) +
    theme(axis.text.x = element_text(hjust = 1))
  
}






