

rownames_to_column <- function(tables) {
  out <- lapply(tables, function(x) {
    if (!is.null(row.names(x))) {
      out <- cbind(Name = row.names(x), x)
      row.names(out) <- NULL
      out
    } else {
      x
    }
  })
  out
}

combine_tables <- function(tables, rownames_to_column = TRUE) {
  
  if (rownames_to_column) tables <- rownames_to_column(tables)
  out <- do.call(rbind, tables)
  if (rownames_to_column) rownames(out) <- NULL
  spanner <- vector("list", length(tables))
  names(spanner) <- names(tables)
  
  l <- lapply(tables, nrow) |> unlist()
  lsum <- cumsum(l)
  row_group <- mapply(function(start, stop) start:stop, start = lsum - l + 1, stop = lsum)
  out <- set_wmisc_attributes(out, row_group = row_group)
  out
  
}


