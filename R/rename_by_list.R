#' Rename variables based on a list of new and old variable names
#'
#' Rename the variable in a dataset based of a provided renaming list. The
#' renaming list can be provided as an Excel file or as a data.frame with at
#' least two columns (one with the source variable names and one with the target
#' variable names). Alternatively, the renaming list can be provided directly as
#' named vectors.
#' @details This function is useful when you want to rename multiple variables
#' in a data frame based on a predefined list of new and old variable names.
#' You can provide the renaming list as an Excel file or as a data.frame,
#' making it easy to manage and update the renaming list externally. The
#' function reads the renaming list, extracts the relevant columns, and applies
#' the renaming to the data frame. It also provides feedback on the number of
#' variables that were renamed.
#'
#' @param data A data frame.
#' @param file A filename of an Excel file or a data.frame containing at least
#'   two columns for renaming (variables to rename and target names).
#' @param to_from When a filename or a data.frame is provided, a named character
#'   with the names of the target  and source variable names (e.g., c("to" =
#'   "from")). When no filename is provided, to_from must be a vector with named
#'   variable names c("to1" = "from1", "to2" = "from2")).
#' @param to When a filename or a data.frame is provided, the name of the column
#'   with the target variable names. When no filename is provided, to must be a
#'   vector with target variable names.
#' @param from When a filename or a data.frame is provided, the name of the
#'   column with the source variable names. When no filename is provided, from
#'   must a vector with source variable names.
#' @details If both `file` and `to_from` are provided, the function will read the
#' renaming list from the specified file and use the columns specified in
#' `to_from` to determine the source and target variable names. If only `file`
#' is provided, the function will use the columns specified in `to` and `from`
#' to determine the source and target variable names.
#' If `file` is NULL and `to_from` is provided, the function will use the named
#' vector in `to_from` to determine the source and target variable names.
#' If no variables in the renaming list match the variable names in the data
#' frame, the function will stop and return an error message indicating that
#' no variables were renamed.
#' @author Juergen Wilbert
#' @return A data frame with renamed variables.
#' @keywords internal
#' @export
#' @examples
#' dat <- data.frame(A = NA, B = NA, C = NA, D = NA)
#' rename_by_list(dat, to = c("albert", "bea"), from = c("A", "B"))
#' rename_by_list(dat, to_from = c("carl" = "C", "daniel" = "D"))
#' dic <- data.frame(old = c("A", "B"), new = c("albert", "bea"))
#' rename_by_list(dat, dic, to_from = c("new" = "old"))
#' rename_by_list(dat, dic, to = "new", from = "old")
#' \dontrun{
#' rename_by_list(dat, "rename_list,xlsx", to_from = c("new" = "old"))
#' rename_by_list(dat, "rename_list,xlsx", to = "new", from = "old")
#' }
rename_by_list <- function(data,
                           file = NULL,
                           to_from = NULL,
                           to = NULL,
                           from = NULL) {
  if (!is.null(file)) {
    if (!inherits(file, "data.frame")) dic <- read_xlsx(file)
    if (inherits(file, "data.frame")) dic <- file
    if (!is.null(to_from)) {
      from <- dic[[to_from]]
      to <- dic[[names(to_from)]]
    } else {
      from <- dic[[from]]
      to <- dic[[to]]
    }
  }

  if (is.null(file) && !is.null(to_from)) {
    from <- to_from
    to <- names(to_from)
  }

  rn <- setNames(from, to)
  rn <- rn[rn %in% names(data)]
  rn <- rn[!is.na(names(rn))]

  if (length(rn) == 0) stop("No variables renamed")

  out <- rename(data, !!rn)

  cat(paste0(length(rn), " variables renamed\n"))

  out
}
