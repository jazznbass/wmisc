#' Create a Contingency Table with One or Two Summary Functions
#'
#' This function creates a contingency table by grouping the data into unique 
#' levels of \code{rows} and \code{columns}, then applies one or more summary 
#' functions (by default \code{mean} and a count via \code{length}) to summarize 
#' the variable of interest (\code{var}). Labels can be assigned manually or 
#' automatically retrieved from variable attributes if available.
#'
#' @param rows A vector or factor for the row grouping variable. If \code{columns} 
#'   and \code{var} are missing, \code{rows} can be a list where its first element 
#'   is the row variable, second is the column variable, and third is the variable 
#'   of interest.
#' @param columns A vector or factor for the column grouping variable. If missing, 
#'   it will be extracted from the second element of \code{rows} (if \code{rows} 
#'   is a list).
#' @param var A numeric vector containing the values to be summarized. If missing, 
#'   it will be extracted from the third element of \code{rows} (if \code{rows} 
#'   is a list).
#' @param fn A named list of functions to apply to \code{var} for each combination 
#'   of \code{rows} and \code{columns}. Defaults to 
#'   \code{list(Mean = mean, n = \(.) length(.))}. If only one function is 
#'   provided, the table will display that result. If two are provided, the results 
#'   will be concatenated (e.g., \code{"Mean (n)"}).
#' @param auto_labels A logical value indicating whether to automatically retrieve 
#'   labels from attributes of \code{rows}, \code{columns}, or \code{var}. 
#'   Defaults to \code{TRUE}.
#' @param var_label An optional character string specifying the label for 
#'   \code{var}.
#' @param rows_label An optional character string specifying the label for 
#'   \code{rows}.
#' @param columns_label An optional character string specifying the label for 
#'   \code{columns}.
#' @param fn_label An optional character vector specifying the labels for 
#'   the functions listed in \code{fn}. If not provided, defaults to the names 
#'   of \code{fn}.
#' @param title An optional character string specifying the title of the table. 
#'   By default, it is constructed from \code{fn_label} and \code{var_label}.
#' @param footnote An optional character string specifying a footnote for the 
#'   table.
#' @param file An optional path to a file where the table output should be saved.
#' @param remove_missing A logical value indicating whether rows with missing 
#'   values (\code{NA}) in \code{var} should be removed. Defaults to \code{TRUE}.
#' @param sort A logical value indicating whether to sort the unique levels of 
#'   \code{rows} and \code{columns}. Defaults to \code{TRUE}.
#' @param ... Additional arguments passed to \code{\link{nice_table}}.
#'
#' @details
#' If \code{rows}, \code{columns}, and \code{var} are provided separately, the 
#' function will group the data by \code{rows} and \code{columns}, then apply the 
#' functions in \code{fn} to \code{var} within each combination of row and column 
#' levels. 
#' 
#' If \code{columns} and \code{var} are not specified, you can instead provide a 
#' list to \code{rows} whose elements are (1) the row variable, (2) the column 
#' variable, and (3) the variable to be summarized.
#'
#' If two functions are provided via \code{fn}, the resulting table will show 
#' the first function's value followed by the second function's value in 
#' parentheses. For example, if the first function is \code{mean} and the second 
#' is a count function, a cell might display \code{"3.45 (10)"}.
#'
#' The function attempts to automatically retrieve labels from the data if 
#' \code{auto_labels = TRUE} and the variables have \code{"label"} attributes. If 
#' labels are not found or if \code{var_label}, \code{rows_label}, or 
#' \code{columns_label} is explicitly specified, those provided labels will be 
#' used.
#'
#' Any \code{NA} values in \code{var} are removed before performing calculations 
#' if \code{remove_missing = TRUE}.
#'
#' By default, the function also sorts the levels of the row and column variables.
#' @return An html table.
#' @examples
#' nice_contingency_table(wmisc:::mtcars_labeled[, c("cyl", "carb", "mpg")])
#' 
#' nice_contingency_table(
#'   wmisc:::mtcars_labeled[, c("cyl", "am", "mpg")],
#'   fn = list(Median = median, "mean average deviation" = mad),
#'   label_na = "-",
#' )
#' 
#' @export
nice_contingency_table <- function(rows, 
                                   columns, 
                                   var, 
                                   fn = list(
                                     Mean = mean, 
                                     n = \(.) length(.)
                                   ),
                                   auto_labels = TRUE,
                                   var_label = NULL,
                                   rows_label = NULL,
                                   columns_label = NULL,
                                   fn_label = NULL,
                                   title = NULL,
                                   footnote = NULL,
                                   file = NULL,
                                   remove_missing = TRUE,
                                   sort = TRUE,
                                   ...) {

  if (missing(columns) && missing(var)) {
    columns <- rows[[2]]
    var <- rows[[3]]
    rows <- rows[[1]]
  }
  
  if (auto_labels && is.null(var_label)) 
    var_label <- get_label(var)
  
  
  if (auto_labels && is.null(columns_label)) 
    columns_label <- get_label(columns)
  
  if (auto_labels && is.null(rows_label)) 
    rows_label <- get_label(rows)
  
  
  if (is.null(var_label)) 
    var_label <- deparse(substitute(var))
  
  if (is.null(fn_label)) 
    fn_label <- names(fn)
  
  if (is.null(rows_label)) 
    rows_label <- deparse(substitute(rows))
  
  if (is.null(columns_label)) 
    columns_label <- deparse(substitute(columns))
  
  
  if (is.null(title)) {
    if (length(fn) == 1) {
      title <- paste0(fn_label, " table for '", var_label, "'")
    } else {
      title <- paste0(fn_label[1], " (", fn_label[2], ") table for '", var_label, "'")
    }
  }

  if (remove_missing) {
    rows <- rows[!is.na(var)]
    columns <- columns[!is.na(var)]
    var <- var[!is.na(var)]
  }
    
  x_levels <- unique(rows)  
  y_levels <- unique(columns)
  
  if (sort) x_levels <- sort(x_levels)
  if (sort) y_levels <- sort(y_levels)

  out <- matrix(
    rep(NA, length(x_levels) * length(y_levels)), 
    nrow = length(x_levels),
    dimnames = list(x_levels, y_levels)
  )
  
  for(row in seq_along(x_levels)) {
    for(col in seq_along(y_levels)) {
      dat <- var[rows == x_levels[row] & columns == y_levels[col]]
      if (length(fn) == 1) {
        out[row, col] <- round(fn[[1]](dat), 2)
      } else {
        value_1 <- round(fn[[1]](dat), 2)
        value_2 <- round(fn[[2]](dat), 2)
        if (is.na(value_2)) {
          value_2 <- ""
        } else {
          value_2 <- paste0("(", value_2, ")")
        }
        if (!is.na(value_1)) {
          out[row, col] <- paste0(value_1, " ", value_2)
        }
      }
      
    }
  }
  
  out <- as.data.frame(out)
  rownames(out) <- NULL
  out <- cbind(Value = x_levels, out)
  names(out)[1] <- rows_label
  
  spanner <- list(var = 2:ncol(out))
  names(spanner)[1] <- columns_label
  
  out <- set_wmisc_attributes(
    out, 
    title = title,
    footnote = footnote,
    file = file,
    spanner = spanner
  )

  nice_table(out, ...)
  
}
