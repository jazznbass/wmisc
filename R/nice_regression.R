#' Create a nice table from one or more regression models
#' @examples
#' lm(mpg ~ am + disp + hp, data = mtcars) |> 
#'   nice_regression_table()
#' nice_regression_table(
#'   wmisc:::model1, wmisc:::model2, 
#'   rename_labels = list(
#'     "EffectTrend" = "Trend", "EffectSlope" = "Slope"), 
#'   rename_cols = list("Estimate" = "b", "SE" = "se")
#' )
#' @export
nice_regression_table <- function(
    ..., 
    digits = 2, 
    label_models = NULL,
    rename_labels = list(),
    rename_cols = list(),
    auto_col_names = TRUE
) {
  
  models <- list(...)
  
  cl <- class(models[[1]])
  n_models <- length(models)
  
  #if (inherits(models[[1]], "lm"))
  models_summary <- lapply(models, summary)
  #if (inherits(models[[1]], "lme"))
  #  models_summary <- models
  
  labels <- list(
    predictor = "Predictor",
    estimate = "Estimate",
    se = "SE",
    statistic = "t",
    p = "p"
  )

  
  get_coef_table <- function(x) {
    out <- as.data.frame(coef(x))
    
    if (auto_col_names) {
      rn <- list(
        Value = label_estimate,
        Estimate = labels$estimate,
        "t-value" = "t",
        "t value" = "t",
        "Std.Error" = labels$se,
        "Std. Error" = labels$se,
        "Pr(>|t|)" = labels$p,
        "t-value" = "t"
      )
      names(out) <- change_by_list(names(out), rn)
    }
    names(out) <- change_by_list(names(out), rename_cols)
    out$p <- nice_p(out$p, stars = TRUE)
    out
  }
  
  out <- lapply(models_summary, \(x) get_coef_table(x))
  n_param <- ncol(out[[1]])
  
  if (length(out) > 1) out <- do.call(combine_cols, out) else out <- out[[1]]

  names(out) <- make.unique(names(out))
  
  cols_label <- as.list(rep(names(out)[1:n_param], length(models)))
  names(cols_label) <- names(out)

  out <- cbind(var = rownames(out), out)
  rownames(out) <- NULL
  
  n_predic <- nrow(out)
  
  add_param <- function(param, label) {
    out[nrow(out) + 1, 1] <- label
    out[nrow(out), 2 + (0:(n_models - 1)) * n_param] <- param
    out
  }
  
  if (cl == "lm") n <- lapply(models, \(x) nrow(x$model)) |> unlist()
  if (cl == "lme") n <- lapply(models, \(x) nrow(x$data)) |> unlist()
  if (cl == "lmerModLmerTest") n <- lapply(models, \(x) length(x@resp$y)) |> unlist()
  
  out <- add_param(n, "N")

  if (cl == "lm") {
    out <- add_param(
      lapply(models_summary, \(x) x$r.squared) |> unlist(), 
      "R\u00b2"
    )
    out <- add_param(
      lapply(models_summary, \(x) x$adj.r.squared) |> unlist(), 
      "R\u00b2 adjusted"
    )
  }

  out <- round_numeric(out, digit = digits)
  
  names(out)[1] <- label_predictor
  
  if (cl == "lmerModLmerTest") {
    auto_labels <- lapply(models, \(x) get_labels(x@frame)) |> unlist()
  }

  if (cl %in% c("lm", "lme")) {
    auto_labels <- lapply(models, \(x) get_labels(x$model)) |> unlist()
  }
  
  auto_labels <- c(unlist(rename_labels), auto_labels)
  
  out[[1]] <- change_by_list(out[[1]], auto_labels)
  
  # spanner ----
  
  spanner <- 2 + (0:(n_models - 1)) * n_param
  spanner <- lapply(spanner, \(x) x:(x + n_param - 1))
  
  if (is.null(label_models)) {
    if (cl == "lmerModLmerTest") {
      label_models <- unlist(
        lapply(models, \(x) x@call$formula[[2]] |> as.character())
      )
    } else {
      label_models <- unlist(
        lapply(models, \(x) x$terms[[2]] |> as.character())
      )
    }

    label_models <- change_by_list(label_models, auto_labels)
    label_models <- make.unique(label_models, sep = " ")
  }
  
  names(spanner) <- label_models
  
  row_group <- list(
    #Coefficients = 1:(n_predic),
    Model = (n_predic + 1):nrow(out)
  )
  
  # out -----
  
  out <- set_wmisc_attributes(out,
    cols_label = cols_label,
    spanner = spanner,
    row_group = row_group,
    label_na = "",
    title = "Regression model"
  )
  nice_table(out) |> 
    gt::row_group_order(groups = c(NA, "Model"))
  
}



