#' Table with alpha values
#'
#' Returns a data.frame with item analyses for the provided scales.
#'
#' @param data A data Frame
#' @param scales A list containing vectors with variable names. Each list
#'   element defines one scale. Named list elements are used as labels.
#' @param labels Label names for scales (defaults to named list elements in
#'   'scales').
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
#' ## Example needs packages scaledic and purrr installed and active
#' nice_alpha_table(
#'   data = wmisc:::ex_itrf,
#'   scales = wmisc:::ex_itrf_scales,
#'   labels = c("Internalizing", "Externalizing"),
#'   keys_from_weights = TRUE,
#'   difficulty = TRUE,
#'   values = list(c(0, 3)),
#'   RMSEA = TRUE
#' )
#'
#' nice_alpha_table(
#'   wmisc:::data_emo,
#'   wmisc:::data_emo_scales,
#'   check_key = TRUE,
#'   difficulty = TRUE,
#'   value = list(c(0,4))
#'   )
#' @export
nice_alpha_table <- function(data,
                             scales,
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
                             fa = TRUE,
                             ...) {
  out <- do.call(alpha_table, as.list(environment()))
  nice_table(out, ...)
}

#' @export
#' @rdname nice_alpha_table
alpha_table <- function(data,
                        scales,
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
  
  
  on.exit(print_messages())
  
  if (!inherits(data, "data.frame")) 
    add_message("Provided data must be of class data.frame")
  
  if (!inherits(scales, "list")) 
    add_message("Scales must be provided in a list")
  
  if (!is.null(keys)) {
    check_key <- FALSE
    keys_from_weights <- FALSE
  }
  
  if (difficulty && is.null(values)) {
    add_message("Can not calculate item difficulty without min and max scale values: values = list(c(min, max))")
    difficulty <- FALSE
  }
  
  if (is.null(labels)) labels <- labels(scales)
  df <- data.frame(Scale = labels)
  
  if (!is.null(values) && (length(values) != length(scales)))
    values <- rep(values, length(scales))
  
  for (i in 1:length(scales)) {
    data_scale <- data[, scales[[i]]]
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
      .id <- which(!scales[[i]] %in% filter_names)
      scales[[i]] <- scales[[i]][.id]
      data_scale <- data_scale[, scales[[i]]]
    }
    
    if (any(is.na(.var), na.rm = TRUE)) {
      filter_names <- names(data_scale)[which(is.na(.var))]
      add_message(
        "Variable with NA variance dropped from analyses: ",
        paste0(filter_names, collapse = ", ")
      )
      .id <- which(!scales[[i]] %in% filter_names)
      scales[[i]] <- scales[[i]][.id]
      data_scale <- data_scale[, scales[[i]]]
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

    if (!is.null(values)) {
      min <- values[[i]][1]
      max <- values[[i]][2]
    }
  
    a <- invisible(suppressWarnings(
      psych::alpha(
        data_scale, 
        check.keys = check_key, 
        keys = keys, 
        use = "pairwise"
      )
    ))
    
    if (fa) f <- invisible(psych::fa(data_scale))
    alpha <- a$total$raw_alpha
    df$"items"[i] <- a$nvar
    df$"cases"[i] <- glue("[{min(a$item.stats$n, na.rm = TRUE)}, {max(a$item.stats$n, na.rm = TRUE)}]")
    
    #sum(complete.cases(data_scale))#min(a$item.stats$n, na.rm = TRUE)
    
    
    if (!ci) df$Alpha[i] <- nice_statnum(alpha, 2)
    
    if (ci) {
      alpha_ci <- .alpha_ci(
        alpha, nrow(data_scale), length(scales[[i]]), conf_level
      )
      df$Raw[i] <- glue(
        "{nice_statnum(alpha, round)} [{nice_statnum(alpha_ci[1], 2)}, ",
        "{nice_statnum(alpha_ci[2], 2)}]"
      )
    }
    
    alpha.std <- a$total$std.alpha
    if (!ci) {
      df$"Standardized"[i] <- nice_statnum(alpha.std, 2)
    }
    
    if (ci) {
      alpha_std_ci <- .alpha_ci(
        alpha.std, nrow(data_scale), length(scales[[i]]), conf_level
      )
      df$"Standardized"[i] <- glue(
        "{nice_statnum(alpha.std, 2)} [{nice_statnum(alpha_std_ci[1], 2)}, ",
        "{nice_statnum(alpha_std_ci[2], 2)}]"
      )
    }
    
    dmin <- round(min(a$item.stats$r.drop), round)
    dmax <- round(max(a$item.stats$r.drop), round)
    mmin <- round(min(a$item.stats$mean), round)
    mmax <- round(max(a$item.stats$mean), round)
    smin <- round(min(a$item.stats$sd), round)
    smax <- round(max(a$item.stats$sd), round)
    if (fa) {
      lmin <- round(min(abs(f$loadings)), round)
      lmax <- round(max(abs(f$loadings)), round)
    } else {
      lmin <- NA
      lmax <- NA
      
    }
    if (difficulty) {
      dif_min <- round((mmin - min) / (max - min), round)
      dif_max <- round((mmax - min) / (max - min), round)
    }
    df$"Homogeneity"[i] <- nice_statnum(a$total$average_r, 2)
    df$"Discriminations"[i] <- glue(
      "[{nice_statnum(dmin, 2)}, {nice_statnum(dmax, 2)}]"
    )
    if (difficulty) {
      df$"Difficulties"[i] <- glue(
        "[{nice_statnum(dif_min, 2)}, {nice_statnum(dif_max, 2)}]"
      )
    }
    df$"Means"[i] <- glue("[{mmin}, {mmax}]")
    df$"SDs"[i] <- glue("[{smin}, {smax}]")
    df$"|Loadings|"[i] <- glue("[{nice_statnum(lmin, 2)}, {nice_statnum(lmax, 2)}]")
    if (RMSEA) df$"RMSEA"[i] <- nice_statnum(f$RMSEA[1], 3)
  }
  
  if (ci) {
    names(df)[which(names(df) == "Alpha")] <- glue("Alpha CI{conf_level*100}%")
    names(df)[which(names(df) == "Std.Alpha")] <- glue(
      "Std.Alph CI{conf_level * 100}%"
    )
  }
  
  df <- set_wmisc_attributes(df, 
                             note = c(
                               "Values in brackets depict upper and lower bound of confidence intervals or [min,max] intervals", 
                               "N cases is the min and max number of non-missing cases for the scale items"),
                             title = "Item analysis",
                             spanner = list(N = 2:3, "Alpha [95% CI]" = 4:5)
  )
  
  df
}



.alpha_ci <- function(alpha, n, items, ci) {
  f <- qf(c(1 - (1 - ci) / 2, (1 - ci) / 2), n - 1, (n - 1) * (items - 1))
  out <- 1 - (1 - alpha) * f
  out
}

