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
    labels_models = NULL,
    rename_labels = list(),
    rename_cols = list(),
    auto_col_names = TRUE
) {
  
  models <- list(...)
  
  class(models) <- c(class(models[[1]]), "list")
  models_params <- extract_model_param(models)
  
  n_models <- length(models)
  
  out <- models_params$estimates$fixed
  n_param <- ncol(out[[1]])

  out <- lapply(out, \(x) {
    x[[models_params$estimates$p_label]] <- 
      nice_p(x[[models_params$estimates$p_label]], stars = TRUE)
    x
  })

  
  # add random ----
  
  # for(i in seq_along(out)) {
  # 
  #   if (!is.null(models_params$estimates$random[[i]])){
  #     random <- models_params$estimates$random[[i]]
  #     
  #     rownames(random) <- random[[1]]
  #    
  #     random <- random[,2, drop = FALSE]
  #     names(random) <- names(out[[i]])[1]
  #     
  #     out[[i]] <- combine_data_frames(out[[i]], random)
  # 
  #   }
  # }
  
  # join tables -----
  
  if (length(out) > 1) out <- do.call(combine_cols, out) else out <- out[[1]]
  
  # auto rename cols ----
  
  labels <- list(
    predictor = "Predictor",
    estimate = "Estimate",
    se = "SE",
    statistic = "t",
    p = "p"
  )
  if (auto_col_names) {
    rn <- list(
      Value = labels$estimate,
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
  names(out) <- make.unique(names(out))
  
  # 
  
  cols_label <- as.list(rep(names(out)[1:n_param], length(models)))
  names(cols_label) <- names(out)
  out <- cbind(var = rownames(out), out)
  rownames(out) <- NULL
  
  n_predic <- nrow(out)

  # add model parameters from extract -----
  
  add_param <- function(param, label) {
    out[nrow(out) + 1, 1] <- label
    out[nrow(out), 2 + (0:(n_models - 1)) * n_param] <- param
    out
  }

 
  for(i in seq_along(models_params$add_param)) {
    out <- add_param(models_params$add_param[[i]], names(models_params$add_param)[i])
  }

  # round and rename cols and predictor labels ----
  
  out <- round_numeric(out, digit = digits)
  
  names(out)[1] <- labels$predictor
  
  auto_labels <- c(unlist(rename_labels), models_params$auto_labels)
  
  out[[1]] <- change_by_list(out[[1]], auto_labels)
  
  # spanner ----
  
  spanner <- 2 + (0:(n_models - 1)) * n_param
  spanner <- lapply(spanner, \(x) x:(x + n_param - 1))

  if (is.null(labels_models)) {
    labels_models <- change_by_list(models_params$labels_models, auto_labels)
    labels_models <- make.unique(labels_models, sep = " ")
  }

  names(spanner) <- labels_models
  
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


extract_model_param <- function (x, ...) {
  UseMethod("extract_model_param", x)
}

extract_model_param.lm <- function(models) {
  
  models_summary <- lapply(models, summary)
  
  out <- list()
  
  out$auto_labels <- lapply(models, \(x) get_labels(x$model)) |> unlist()
  out$labels_models <-
    lapply(models, \(x) x$terms[[2]] |> as.character()) |> 
    unlist()
  
  out$add_param$N <- lapply(models, \(x) nrow(x$model)) |> unlist()
  out$add_param[["R\u00b2"]] <- 
    lapply(models_summary, \(x) x$r.squared) |> unlist()
  out$add_param[["R\u00b2 adjusted"]] <- 
    lapply(models_summary, \(x) x$adj.r.squared) |> unlist()

  
  get_coef_table <- function(x) {
    out <- as.data.frame(coef(x))
    out
  }
  
  out$estimates$fixed <- lapply(models_summary, \(x) get_coef_table(x))
  out$estimates$p_label <- names(out$estimates$fixed[[1]])[ncol(out$estimates$fixed[[1]])]
  out$estimates$random <- NULL

  out
}

extract_model_param.lme <- function(models) {
  
  models_summary <- lapply(models, summary)
  
  out <- list()
  out$add_param$N <- lapply(models, \(x) nrow(x$data)) |> unlist()
  out$auto_labels <- lapply(models, \(x) get_labels(x$model)) |> unlist()
  out$labels_models <-
    lapply(models, \(x) x$terms[[2]] |> as.character()) |> 
    unlist()
  
  get_coef_table <- function(x) {
    out <- as.data.frame(coef(x))
    out
  }
  
  out$estimates$fixed <- lapply(models_summary, \(x) get_coef_table(x))
  out$estimates$p_label <- names(out$estimates$fixed[[1]])[ncol(out$estimates$fixed[[1]])]
  out$estimates$random <- NULL
  
  out
}

extract_model_param.lmerModLmerTest <- function(models) {
  models_summary <- lapply(models, summary)
  out <- list()

  out$auto_labels <- lapply(models, \(x) get_labels(x@frame)) |> unlist()
  
  out$labels_models <- 
    lapply(models, \(x) x@call$formula[[2]] |> as.character()) |> 
    unlist()
  
  get_coef_table <- function(x) {
    out <- as.data.frame(coef(x))
    out
  }
  out$estimates$fixed <- lapply(models_summary, \(x) get_coef_table(x))
  out$estimates$p_label <- 
    names(out$estimates$fixed[[1]])[ncol(out$estimates$fixed[[1]])]
  
  get_random <- function(x) {
    random <- x$varcor |> as.data.frame()
    names(random)[1] <- "Effect"
    n_effects <- nrow(random) - 1 
    var_effects <- sum(random$vcov[1:n_effects])
    var_intercept_effects <- 
      sum(random$vcov[which(random$var1 == "(Intercept)")])
    var_total <- sum(random$vcov)
    random$Partial_explained <- NA
    random$Partial_explained[1:n_effects] <- 
      random$vcov[1:n_effects] /  var_effects
    random$Total_explained <- NA
    random$Total_explained <- random$vcov /  var_total
    
    random$Effect[1:n_effects] <- 
      paste0(random[[2]][1:n_effects], " ", random[[1]][1:n_effects])
    random <- random[nrow(random):1, c(1,4)]
    
    random[nrow(random) + 1, "Effect"] <- "ICC"
    random[nrow(random), 2] <- var_intercept_effects / var_total
    
    #names(random) <- c("Effect", "")
    random
  }
  
  random <- lapply(models_summary, get_random)

  for(i in 1:length(random)) {
    for(j in 1:nrow(random[[i]])) {
      if (is.null(out$add_param[[random[[i]][j ,1]]]))
        out$add_param[[random[[i]][j ,1]]] <- rep(NA, length(random))
      out$add_param[[random[[i]][j ,1]]][i] <- random[[i]][j ,2]
    }
  }
  
  id <- which(!names(out$add_param) %in% c("Residual", "ICC"))
  new <- c(names(out$add_param)[id], "Residual", "ICC")
  out$add_param <- out$add_param[new]
  
  
  out$add_param$N <- lapply(models, \(x) length(x@resp$y)) |> unlist()
  
  out
}



