#' Create a Percentage Bar Chart
#'
#' This function creates a percentage bar chart using ggplot2, where the user can specify
#' the percentage to be represented as a bar along with an optional label.
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
  ggplot(data, aes(x = Year, y = Proportion, fill = Group)) +
    geom_col() +
    geom_text(aes(label = label), size = 10,
              position = position_stack(vjust = 0), hjust = 0) +
    scale_fill_brewer(palette = "Set4") +
    theme_minimal(base_size = 18) +
    ylab("Percentage") +
    xlab(NULL) + ylab(NULL) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank()
    ) +
    coord_flip() 
}



