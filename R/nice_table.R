#' Create a nicely formatted table
#' 
#' This function takes a data frame and formats it into a nicely
#' formatted HTML table using the `knitr` and `kableExtra` packages.
#' 
#' @param x The data frame to be formatted into a table
#' @param ... Additional arguments passed to `knitr::kable()`
#' @param extra Additional arguments passed to `kableExtra::kable_classic_2()`
#' @param title Title string.
#' @param footnote Add footnote
#' @return A nicely formatted HTML table
#' 
#' 
#' @examples
#' df <- data.frame(x = 1:5, y = rnorm(5))
#' nice_table(df, extra = list(full_width = FALSE))
#' 
#' @export
nice_table <- function(x, 
                       ..., 
                       extra = NULL, 
                       title = NULL, 
                       footnote = NULL, 
                       engine = "extra", 
                       rownames = FALSE) {
  
  if (!is.null(attr(x, "wmisc_title")) && is.null(title)) title <- attr(x, "wmisc_title")
  if (!is.null(attr(x, "wmisc_note")) && is.null(footnote)) footnote <- attr(x, "wmisc_note")
  if (!is.null(title)) title <- paste0("Table.<br><i>", title, "</i>")
  
  if (engine == "extra") {
    
    x <- knitr::kable(
      x, 
      caption = title, 
      align = c("l", rep("c", ncol(x) - 1)),  
      ...
    )
    out <- do.call(kableExtra::kable_classic, c(list(x), extra)) 
    if (!is.null(footnote)) out <- kableExtra::footnote(out, footnote)
  }

  if (engine == "gt") {
    if (!inherits(x, "data.frame")) {
      x <- as.data.frame(x)
      rownames(x) <- NULL
    }
    if (rownames && !is.null(rownames(x))) x <- cbind(" " = rownames(x), x)
    if (!is.null(title)) title <- gt::html(title)
    out <- gt::gt(x, caption = title) |> 
      .gt_apa_style()
    if (!is.null(footnote)) out <- gt::tab_footnote(out, footnote)
    
    
  }
  
  out
}


.gt_apa_style <- function(gt_tbl) {
  gt_tbl  |> 
    tab_options(
      table.border.bottom.color = "white",
      #table.border.bottom.width = 3,
      
      table.border.top.color = "black",
      table.border.top.width = 3,
      
      table_body.border.bottom.color = "black",
      table_body.border.bottom.width = 3,
      
      table_body.hlines.width = 0,
      
      
      table_body.hlines.width = 0,
      
      column_labels.border.bottom.width = 2,
      column_labels.border.bottom.color = "black",
    )  |> 
    opt_table_font(font = "times") |> 
    gt::cols_align(align = "center") |> 
    gt::cols_align(align = "left", columns = 1)
}
