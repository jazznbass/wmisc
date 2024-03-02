#' Create a nicely formatted table
#'
#' This function takes a data frame and formats it into a nicely formatted HTML
#' table using the `gt` packages.
#'
#' @param x The data frame to be formatted into a table
#' @param title Title string.
#' @param footnote Add footnote
#' @param file (works only when `engine = "gt"`) If set, an additional file with
#'   the table is produced.
#' @param cols_label List with renaming information for columns (old_name =
#'   new_name).
#' @param row_group List with information on grouping rows `row_group =
#'   list("Start" = 1:2, "That is the second" = 3:5)`
#' @param row_group_order List with information on grouping order.
#' @param decimals Number of decimals that will be printed.
#' @param round Number of digits to which numbers should be rounded.
#' @param extra Additional arguments passed to `kableExtra::kable_classic_2()`
#' @param gt Additional arguments passed to `gt::gt()`
#' @param kable Additional arguments passed to `knitr::kable()`
#' @return A nicely formatted HTML table
#'
#'
#' @examples
#' df <- data.frame(
#'   x = 1:5, y = rnorm(5, mean = 10, sd = 12),
#'   c = letters[5:1], d = sample(letters, 5)
#' )
#' nice_table(
#'   df,
#'   title = "A nice title",
#'   footnote = c("Footnote 1", "Footnote 2"),
#'   spanner = list("One" = 1:2, "Two" = 3:4),
#'   row_group = list("Start" = 1:2, "That is the second" = 3:5),
#'   cols_label = list(x = "First", y = "Second", c = "Third", d = "Fourth"),
#'   decimals = 1
#' )
#'
#' @export
nice_table <- function(x, 
                       title = NULL, 
                       footnote = NULL, 
                       spanner = header,
                       row_group = NULL,
                       row_group_order = NULL,
                       header = NULL,
                       pack = NULL,
                       rownames = FALSE,
                       file = NULL,
                       cols_label = NULL,
                       use_labels = TRUE,
                       decimals = NULL,
                       round = NULL,
                       label_na = NULL,
                       engine = getOption("wmisc.nice.table.engine"),
                       extra = NULL, 
                       gt = NULL,
                       kable = NULL) {
  
  args <- get_wmisc_attributes(x)
  
  if (!is.null(args)) {
    if (!is.null(args$spanner) && is.null(spanner)) 
      spanner <- args$spanner
    if (!is.null(args$row_group) && is.null(row_group)) 
      row_group <- args$row_group
    if (!is.null(args$cols_label) && is.null(cols_label)) 
      cols_label <- args$cols_label
    if (!is.null(args$file) && is.null(file)) 
      file <- args$file
    if (!is.null(args$title) && is.null(title)) 
      title <- args$title
    if (!is.null(args$note) && is.null(footnote)) 
      footnote <- args$note
    if (!is.null(args$label_na) && is.null(label_na)) 
      label_na <- args$label_na
    if (!is.null(args$row_group_order) && is.null(row_group_order)) 
      row_group_order <- args$row_group_order
  }
  
  if (!is.null(pack)) row_group <- pack
  
  if (isTRUE(file)) {
    file <- gsub(" ", "-" , x = title)
    file <- gsub("\\.", "" , x = file)
    file <- tolower(file)
    file <- paste0("tab-", file, ".docx")
  } 
  
  if (!is.null(title)) title <- paste0("**Table**<br>  *", title, "*")
  if (!is.null(footnote)) {
    footnote <- paste0("*Note.* ", paste0(footnote, collapse = ". "), ".")
  }
  
  x <- round_numeric(x, round)
  
  if (FALSE) {
    new_cols_label <- lapply(x, \(x) attr(x, "label")) 
    new_cols_label <- new_cols_label[which(!is.null(new_cols_label))]
    cols_label <- c(cols_label, new_cols_label)
    cols_label <- cols_label[which(!duplicated(names(cols_label)))]
  }
  
  
  if (engine == "extra") 
    out <- .nice_table_kable(x, title, row_group, spanner, footnote)

  if (engine == "gt") {
    if (!inherits(x, "data.frame")) {
      x <- as.data.frame(x)
      rownames(x) <- NULL
    }
    if (rownames && !is.null(rownames(x))) x <- cbind(" " = rownames(x), x)
    
    args <- c(list(data = x), gt)
    
    out <- do.call(gt::gt, args)|> gt_apa_style()

    if (!is.null(title)) out <- gt::tab_header(out, title = gt::md(title))
    if (!is.null(row_group)) {
      for(i in length(row_group):1)
        out <- gt::tab_row_group(
          out, label = names(row_group)[i], rows = row_group[[i]]
        )
      for(i in length(row_group):1)  
        out <- gt::tab_style(
          out, style = gt::cell_text(align = "center"),
          locations = gt::cells_row_groups(groups = names(row_group)[i])
        )
    }
    
    if (!is.null(row_group_order)) {
      out <- gt::row_group_order(out, groups = row_group_order)
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
    if (!is.null(decimals)) out <- gt::fmt_number(out, decimals = decimals)
    if (!is.null(label_na)) out <- gt::sub_missing(out, missing_text = label_na)
    
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
      column_labels.border.top.color = "black",
      
      row_group.border.bottom.color = "white",
      row_group.border.bottom.style = NULL,
      row_group.border.bottom.width = NULL
      
    )  |> 
    opt_table_font(font = "times") |> 
    gt::cols_align(align = "center") |> 
    gt::cols_align(align = "left", columns = 1)
}

.nice_table_kable <- function(x, title, row_group, spanner, footnote) {

    args <- c(
      list(x = x, caption = title, align = c("l", rep("c", ncol(x) - 1))), 
      kable
    )
    x <- do.call(knitr::kable, args)
    out <- do.call(kableExtra::kable_classic, c(list(x), extra)) 
    
    if (!is.null(row_group)) {
      out <- kableExtra::pack_rows(out, index = row_group, bold = FALSE)
    }
    
    if (!is.null(spanner)) {
      out <- kableExtra::add_header_above(out, spanner)
    }
    
    if (!is.null(footnote)) out <- kableExtra::footnote(out, footnote)
    
    out
}
