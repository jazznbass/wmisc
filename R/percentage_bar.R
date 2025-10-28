#' Create a Percentage Bar Chart
#'
#' This function creates a percentage bar chart using ggplot2, where the user
#' can specify the percentage to be represented as a bar along with an optional
#' label.
#'
#' @param perc The percentage value to be represented as a bar in the chart.
#' @param label An optional label to be displayed along with the percentage bar.
#'
#' @return A ggplot2 bar chart representing the given percentage value.
#' @examples
#' # Create a percentage bar chart with 30% labeled as "Completed"
#' percentage_bar(30, "Completed")
#'
#' @export
percentage_bar <- function(perc, label) {
  data <- data.frame(
    Group = factor(c("A", "B")), 
    Proportion = c(100-perc,perc))
  label <- c("", paste0(perc, "%  ", label))
  data <- data %>% mutate(Year = "")
  ggplot2::ggplot(data, ggplot2::aes(x = Year, y = Proportion, fill = Group)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 10,
              position = position_stack(vjust = 0), hjust = 0) +
    ggplot2::scale_fill_brewer(palette = "Set4") +
    ggplot2::theme_minimal(base_size = 18) +
    ggplot2::xlab(NULL) + 
    ggplot2::ylab(NULL) +
    ggplot2::theme(
      axis.line=ggplot2::element_blank(),
      axis.text.x=ggplot2::element_blank(),
      axis.text.y=ggplot2::element_blank(),
      axis.ticks=ggplot2::element_blank(),
      axis.title.x=ggplot2::element_blank(),
      axis.title.y=element_blank(),
      legend.position="none",
      panel.background=ggplot2::element_blank(),
      panel.border=ggplot2::element_blank(),
      panel.grid.major=ggplot2::element_blank(),
      panel.grid.minor=ggplot2::element_blank(),
      plot.background=ggplot2::element_blank()
    ) +
    ggplot2::coord_flip() 
}



