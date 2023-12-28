#' Create a nicely formatted table
#'
#' This function takes a data frame and formats it into a nicely formatted HTML
#' table using the `knitr` and `kableExtra` packages.
#'
#' @param x The data frame to be formatted into a table
#' @param ... Additional arguments passed to `knitr::kable()`
#' @param extra Additional arguments passed to `kableExtra::kable_classic_2()`
#' @param title Title string.
#' @param footnote Add footnote
#' @param file (works only when `engine = "gt"`) If set, an additional file with
#'   the table is produced.
#' @param cols_label List with renaming information for columns (old_name = new_name)
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
                       engine = getOption("wmisc.nice.table.engine"),
                       header = NULL,
                       spanner = header,
                       pack = NULL,
                       rownames = FALSE,
                       file = NULL,
                       cols_label = NULL) {
  
  if (!is.null(attr(x, "wmisc_title")) && is.null(title)) {
    title <- attr(x, "wmisc_title")
  }
  if (!is.null(attr(x, "wmisc_note")) && is.null(footnote)) {
    footnote <- attr(x, "wmisc_note")
  }
  
  if (!is.null(title)) title <- paste0("**Table**<br>  *", title, "*")
  if (!is.null(footnote)) footnote <- paste0("*Note.* ", footnote)
  
  if (engine == "extra") {
    
    x <- knitr::kable(
      x, 
      caption = title, 
      align = c("l", rep("c", ncol(x) - 1)),  
      ...
    )
    out <- do.call(kableExtra::kable_classic, c(list(x), extra)) 
    
    if (!is.null(pack)) {
      out <- kableExtra::pack_rows(out, index = pack, bold = FALSE)
    }
    
    if (!is.null(spanner)) {
      out <- kableExtra::add_header_above(out, spanner)
    }
    
    if (!is.null(footnote)) out <- kableExtra::footnote(out, footnote)
  }

  if (engine == "gt") {
    if (!inherits(x, "data.frame")) {
      x <- as.data.frame(x)
      rownames(x) <- NULL
    }
    if (rownames && !is.null(rownames(x))) x <- cbind(" " = rownames(x), x)
    
    out <- gt::gt(x, ...) |> gt_apa_style()
    
    if (!is.null(title)) out <- gt::tab_header(out, title = gt::md(title))
    if (!is.null(pack)) {
      for(i in length(pack):1)
        out <- gt::tab_row_group(out, label = names(pack)[i], rows = pack[[i]])
      #gt::row_group_order(names(pack)))
    }
    if (!is.null(spanner)) {
      for(i in seq_along(spanner)) {
        out <- gt::tab_spanner(
          out, 
          label = names(spanner)[i], 
          columns = spanner[[i]]
        )  
      }
    }
    
    if (!is.null(cols_label)) out <- gt::cols_label(out, .list = cols_label)
    if (!is.null(footnote)) out <- gt::tab_footnote(out, gt::md(footnote))
    
    if (!is.null(file)) gt::gtsave(out, file)
    
  }
  
  out
}

#' @export
gt_apa_style <- function(gt_tbl) {
  gt_tbl  |> 
    tab_options(
      table.border.bottom.color = "white",
      #table.border.bottom.width = 3,
      
      table.border.top.color = "white",
      #table.border.top.width = 3,
      
      table_body.border.bottom.color = "black",
      table_body.border.bottom.width = 3,
      
      table_body.border.top.color = "black",
      table_body.border.top.width = 3,
      
      table_body.hlines.width = 0,
      
      heading.align = "left",
      heading.border.bottom.width = 3,
      heading.border.bottom.color = "black",
      heading.title.font.size = "100%",
      column_labels.border.bottom.width = 2,
      column_labels.border.bottom.color = "black",
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black"
    )  |> 
    opt_table_font(font = "times") |> 
    gt::cols_align(align = "center") |> 
    gt::cols_align(align = "left", columns = 1)
}
