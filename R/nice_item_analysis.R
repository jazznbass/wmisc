#' Table with detailed item statistics for one scale
#'
#' Returns a data.frame with detailed item analyses for the provided scale.
#'
#' @param data A data Frame
#' @param scale A vector with variable names that define the scale.
#' @param round Rounds values to given decimal position.
#' @param ci If TRUE confidence intervals are calculated.
#' @param conf_level Confidence level (e.g. 0.95 for 95 percent).
#' @param check_key Check_key for the psych::alpha function.
#' @param keys Optional key argument for the psych::alpha function.
#' @param keys_from_weights If TRUE, tries to define keys from the weights
#'   dictionary attribute. These are only available when you defined them with 
#'   the scaledic package.
#' @param RMSEA If TRUE RMSEA is calculated.
#' @param difficulty If TRUE, the difficulty of the item is calculated.
#' @param values Sets maximum and minimum valid values necessary to calculate
#'   item difficulty.
#' @param fa If TRUE, a one factor exploratory factor analyses is calculated and
#'   loadings are reported.
#' @return A data frame with concise scale indices.
#' @examples
#' nice_item_analysis(
#'   wmisc:::data_emo, 
#'   scale = wmisc:::data_emo_scales[[1]], 
#'   difficulty = TRUE, 
#'   values = c(1,5)
#' )
#' @export
nice_item_analysis <- function(data,
                       scale,
                       labels = NULL,
                       round = 2,
                       ci = TRUE,
                       conf_level = 0.95,
                       check_key = TRUE,
                       keys = NULL,
                       keys_from_weights = FALSE,
                       RMSEA = FALSE,
                       difficulty = FALSE,
                       values = NULL,
                       fa = TRUE) {
  out <- do.call(item_analysis, as.list(environment()))
  nice_table(out)
}

#' @export
#' @rdname nice_alpha_table
item_analysis <- function(data,
                        scale,
                        labels,
                        round = 2,
                        ci = TRUE,
                        conf_level = 0.95,
                        check_key = TRUE,
                        keys = NULL,
                        keys_from_weights = FALSE,
                        RMSEA = FALSE,
                        difficulty = FALSE,
                        values = NULL,
                        fa = TRUE) {
  
  
  on.exit(print_messages())
  
  if (!inherits(data, "data.frame")) 
    add_message("Provided data must be of class data.frame")
  
  #if (!inherits(scales, "list")) 
  #  add_message("Scales must be provided in a list")
  
  if (!is.null(keys)) {
    check_key <- FALSE
    keys_from_weights <- FALSE
  }
  
  if (difficulty && is.null(values)) {
    add_message("Can not calculate item difficulty without min and max scale values: values = list(c(min, max))")
    difficulty <- FALSE
  }
  
  if (!is.null(values) && (length(values) != length(scale)))
    values <- rep(values, length(scale))
  
  
                                       
  data_scale <- data[, scale]
    
  .id <- apply(data_scale, 1, function(x) all(is.na(x))) |> which()
  if (length(.id) > 0) {
    add_message(
      "Removed ", length(id), " rows because all items were missing."
    )
    data_scale <- data_scale[-.id, ]
  }
  
  .var <- apply(data_scale, 2, function(x) var(x, na.rm = TRUE))
  
  if (any(.var == 0, na.rm = TRUE)) {
    filter_names <- names(data_scale)[which(.var == 0)]
    add_message(
      "Variable with no variance dropped from analyses: ",
      paste0(filter_names, collapse = ", ")
    )
    .id <- which(!scale %in% filter_names)
    scale <- scale[.id]
    data_scale <- data_scale[, scale]
  }
  
  if (any(is.na(.var), na.rm = TRUE)) {
    filter_names <- names(data_scale)[which(is.na(.var))]
    add_message(
      "Variable with NA variance dropped from analyses: ",
      paste0(filter_names, collapse = ", ")
    )
    .id <- which(!scale %in% filter_names)
    scale <- scale[.id]
    data_scale <- data_scale[, scale]
  }
  
  if (keys_from_weights) {
    if (requireNamespace("scaledic", quietly = TRUE)) {
      keys <- lapply(
        data_scale, 
        function(.) as.numeric(scaledic::dic_attr(., "weight"))
      ) |> 
        unlist() |> 
        sign()
      if (identical(length(keys), 0L)) {
        add_message("Weights from scaledic attributes are missing.")
        keys_from_weights <- FALSE
      }
    } else {
      keys <- NULL
      add_message("Scaledic is not installed, keys can not be extracted automatically.")
    }

  }

  a <- invisible(suppressWarnings(
    psych::alpha(
      data_scale, 
      check.keys = check_key, 
      keys = keys, 
      use = "pairwise"
    )
  ))

  
  df <- a$item.stats[, c("n", "r.drop", "mean", "sd")]
  
  if (fa) f <- invisible(psych::fa(data_scale))
  df$loadings <- as.numeric(loadings(f))

  alpha <- a$total$raw_alpha
 
 
  if (!ci) alpha <- nice_statnum(alpha, 2)
  
  if (ci) {
    alpha_ci <- .alpha_ci(
      alpha, nrow(data_scale), length(scale), conf_level
    )
    alpha <- glue(
      "{nice_statnum(alpha, round)} [{nice_statnum(alpha_ci[1], 2)}, ",
      "{nice_statnum(alpha_ci[2], 2)}]"
    )
  }
 
  alpha.std <- a$total$std.alpha
  if (!ci) {
    alpha.std <- nice_statnum(alpha.std, 2)
  }
  
  if (ci) {
    alpha_std_ci <- .alpha_ci(
      alpha.std, nrow(data_scale), length(scale), conf_level
    )
    alpha.std <- glue(
      "{nice_statnum(alpha.std, 2)} [{nice_statnum(alpha_std_ci[1], 2)}, ",
      "{nice_statnum(alpha_std_ci[2], 2)}]"
    )
  }
 
  if (difficulty) {
    df$Difficulty <- (df$mean - values[1]) / (values[2] - values[1])
  }
  #df$"Homogeneity" <- nice_statnum(a$total$average_r, 2)
  if (RMSEA) rmsea <- nice_statnum(f$RMSEA[1], 3) else rmsea <- NA
  
  df$Labels <- if (is.null(labels)) labels <- row.names(df) else labels
  
  rownames(df) <- NULL
  
  ord <- c("Labels", "n", "mean", "sd", "r.drop")
  
  df <- df[, c(ord, names(df)[!names(df) %in% ord])]
  
  
  cols_label <- list(
    "mean" = "M",
    "sd" = "SD",
    r.drop = "Discrimination",
    "loadings" = "Loadings"
  )
  
  df <- set_wmisc_attributes(df, 
                             note = c(),
                             title = "Item analysis",
                             cols_label = cols_label,
                             round = round
                             #spanner = list(N = 2:3, "Alpha [95% CI]" = 4:5)
  )
  
  df
}
