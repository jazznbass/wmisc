
#' @export
nice_table <- function(x, ...) {
  x |> 
    knitr::kable(...)  |>  
    kableExtra::kable_classic_2()
}
