#' Create a nice table from one or more regression models
#' 
#' @param remove_cols Either column number or column names to be removed
#' @param or If TRUE, the estimators are assumed to be logits and are
#'   exponentiated to yield odds ratios
#' @examples
#' lm(mpg ~ am + disp + hp, data = mtcars) |> 
#'   nice_regression_table()
#'   
#' nice_regression_table(
#'   nlme::lme(mpg~disp, data = mtcars, random = ~1|am),
#'   nlme::lme(mpg~disp + hp, data = mtcars, random = ~1|am)
#' )   
#'   
#' nice_regression_table(
#'   wmisc:::model_lmer_1, wmisc:::model_lmer_2,
#'   rename_labels = list(
#'     "EffectTrend" = "Trend", "EffectSlope" = "Slope", "TimePost" = "Post", 
#'     "ConditionTraining" = "Training", "id_subject" = "Subject"),
#'   rename_cols = list("Estimate" = "B", "SE" = "se"),
#'   labels_models = c("Only pretest", "Pre- and posttest") 
#' )
#' @export
nice_regression_table <- function(
    ..., 
    round = 2, 
    labels_models = NULL,
    rename_labels = list(),
    rename_cols = list(),
    remove_cols = NULL,
    auto_col_names = TRUE,
    file = NULL,
    or = FALSE,
    nice_p = TRUE,
    title = "Regression model",
    footnote = NULL
) {
  
  models <- list(...)
  models_params <- lapply(models, \(x) extract_model_param(x, or = or))
  
  n_models <- length(models)
  
  out <- lapply(models_params, \(x) x$estimates$fixed)

  
  if (nice_p) {
    p_label <- models_params[[1]]$estimates$p_label
    out <- lapply(out, \(x) {
      x[[p_label]] <- nice_p(x[[p_label]], stars = TRUE)
      x
    })
  }

  # remove cols ----
  
  if (!is.null(remove_cols)) {
    remove_cols <- unlist(remove_cols)
    if (is.character(remove_cols))
      remove_cols <- which(names(out[[1]]) %in% remove_cols)
    out <- lapply(out, \(x) {
      x[, -remove_cols]
    })
  }
  
  n_param <- ncol(out[[1]])
  
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
      "Pr(>|z|)" = labels$p,
      "z-value" = "z",
      "z value" = "z"
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

  all_params <- lapply(models_params, \(x) x$add_param)
  
  new_params <- lapply(all_params, \(x) names(x)) |> 
    unlist() |> 
    unique() |> 
    sapply(\(x) rep(NA, n_models), simplify = FALSE)

  for(i in seq_along(all_params)) {
    names_params <- names(all_params[[i]])
    for(j in 1:length(names_params)) {
      new_params[[names_params[j]]][i] <- all_params[[i]][[names_params[j]]]
    }
  }
  
  ## sort output params
  names_params <- names(new_params)
  names_params[startsWith(names_params, "n ")]

  vars_end <- c(
    "Residual", "ICC", 
    names_params[startsWith(names_params, "N ")],
    "R\u00b2 Conditional", "R\u00b2 Marginal",
    "Observations"
  )
  id <- sapply(vars_end, \(x) which(names_params %in% x)) |> as.numeric()
  id <- id[!is.na(id)]    
  new_params <- new_params[c(names_params[-id], names_params[id])]
  
  for(i in seq_along(new_params)) {
    out <- add_param(new_params[[i]], names(new_params)[i])
  }

  # round and rename cols and predictor labels ----
  
  out <- round_numeric(out, digit = round)
  
  names(out)[1] <- labels$predictor
  
  auto_labels <- c(unlist(rename_labels), models_params$auto_labels)
  
  for(i in seq_along(auto_labels)) 
    out[[1]] <- gsub(names(auto_labels)[i], auto_labels[i],  out[[1]])
 
  out <- lapply(out, function(x) as.character(x)) |> as.data.frame(check.names = FALSE)
  
  # spanner ----
  
  spanner <- 2 + (0:(n_models - 1)) * n_param
  spanner <- lapply(spanner, \(x) x:(x + n_param - 1))

  if (is.null(labels_models)) {
    labels_models <- change_by_list(
      lapply(models_params, \(x) x$labels_models) |> unlist(), 
      auto_labels
    )
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
    row_group_order = c(NA, "Model"),
    label_na = "",
    title = title,
    footnote = footnote
  )
  nice_table(out, file = file) 
  
}

#' @export
extract_model_param <- function (model, ...) {
  UseMethod("extract_model_param", model)
}

#' @export
extract_model_param.lm <- function(model, or = FALSE, ...) {
  
  model_summary <- summary(model)
  
  out <- list()
  
  out$auto_labels <- get_labels(model$model)
  out$labels_models <- model$terms[[2]] |> as.character()
  
  out$add_param$Observations <- nrow(model$model)
  out$add_param[["R\u00b2"]] <- model_summary$r.squared
  out$add_param[["R\u00b2 adjusted"]] <- model_summary$adj.r.squared

  if (inherits(model, "glm")) out$add_param$"R\u00b2 Tjur" <-  performance::r2_tjur(model)
  
  out$estimates$fixed <- as.data.frame(coef(model_summary))
  out$estimates$p_label <- names(out$estimates$fixed)[ncol(out$estimates$fixed)]
  
  if (or) {
    out$estimates$fixed[[1]] <- exp(out$estimates$fixed[[1]])
  }  
  
  out
}

#' @export
extract_model_param.lme <- function(model, ...) {
  
  model_summary <- summary(model)
  
  out <- list()

  out$auto_labels <- get_labels(model$model)
  out$labels_models <- model$terms[[2]] |> as.character()
  
  out$estimates$fixed <- as.data.frame(coef(model_summary))
  out$estimates$p_label <- names(out$estimates$fixed)[ncol(out$estimates$fixed)]

  varcov <- nlme::getVarCov(model)
  random <- cbind(
    paste0(names(varcov |> diag()), " ",  attr(varcov, "group.levels")) |> as.data.frame(), 
    varcov |> diag() 
  )
  
  
  for(j in 1:nrow(random)) {
    out$add_param[[random[j ,1]]] <- random[j ,2]
  }
  
  out$add_param$Residual <- model$sigma^2
  out$add_param$ICC <- sum(random[[2]]) / (sum(random[[2]]) + model$sigma^2)
  
  performance <- performance::model_performance(model)
  out$add_param$ICC <- performance$ICC
  out$add_param$"R\u00b2 Conditional" <-  performance$"R2_conditional"
  out$add_param$"R\u00b2 Marginal" <-  performance$"R2_marginal"
  
  random_vars <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)
  n_random <- lapply(random_vars, \(x) model$data[[x]] |> unique() |> length()) |> unlist()
  for(i in seq_along(n_random)) {
    out$add_param[[paste0("N ", random_vars[i])]] <- n_random[i]
  }
  #out$add_param$N <- model$dims$ngrps[1:model$dims$Q]
  out$add_param$Observations <- model$dims$N
  
  out
}

#' @export
extract_model_param.lmerModLmerTest <- function(model, ...) {
  tmp <- c(...)
  model_summary <- summary(model)
  out <- list()

  out$auto_labels <- get_labels(model@frame)
  out$labels_models <- model@call$formula[[2]] |> as.character() 
  out$estimates$fixed <- as.data.frame(coef(model_summary))
  out$estimates$p_label <- 
    names(out$estimates$fixed)[ncol(out$estimates$fixed)]
  
  # random effect
  random <- model_summary$varcor |> as.data.frame()
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
  
  #random[nrow(random) + 1, "Effect"] <- "ICC"
  #random[nrow(random), 2] <- var_intercept_effects / var_total

  for(j in 1:nrow(random)) {
    out$add_param[[random[j ,1]]] <- random[j ,2]
  }
  
  performance <- performance::model_performance(model)
  
  out$add_param$ICC <- performance$ICC
  out$add_param$"R\u00b2 Conditional" <-  performance$"R2_conditional"
  out$add_param$"R\u00b2 Marginal" <-  performance$"R2_marginal"

  random_vars <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)
  n_random <- lapply(random_vars, \(x) model@frame[[x]] |> unique() |> length()) |> unlist()
  for(i in seq_along(n_random)) {
    out$add_param[[paste0("N ", random_vars[i])]] <- n_random[i]
  }
  
  out$add_param$Observations <- length(model@resp$y)
  
  
  out
}

#' @export
extract_model_param.glmerMod <- function(model, or = FALSE) {
  model_summary <- summary(model)
  out <- list()
  
  out$auto_labels <- get_labels(model@frame)
  out$labels_models <- model@call$formula[[2]] |> as.character() 
  out$estimates$fixed <- as.data.frame(coef(model_summary))
 
  if (or) {
    out$estimates$fixed[[1]] <- exp(out$estimates$fixed[[1]])
    #out$estimates$fixed[[2]] <- exp(out$estimates$fixed[[2]])
  }  
  
  out$estimates$p_label <- 
    names(out$estimates$fixed)[ncol(out$estimates$fixed)]
  
  # random effect
  random <- model_summary$varcor |> as.data.frame()
  names(random)[1] <- "Effect"
  n_effects <- nrow(random) 
  var_effects <- sum(random$vcov[1:n_effects])
  var_intercept_effects <- 
    sum(random$vcov[which(random$var1 == "(Intercept)")])
  #var_total <- sum(random$vcov)
  #random$Partial_explained <- NA
  #random$Partial_explained[1:n_effects] <- 
  #  random$vcov[1:n_effects] /  var_effects
  #random$Total_explained <- NA
  #random$Total_explained <- random$vcov /  var_total
  random$Effect[1:n_effects] <- 
    paste0(random[[2]][1:n_effects], " ", random[[1]][1:n_effects])
  random <- random[nrow(random):1, c(1,4)]
  
  #random[nrow(random) + 1, "Effect"] <- "ICC"
  #random[nrow(random), 2] <- var_intercept_effects / var_total
  
  for(j in 1:nrow(random)) {
    out$add_param[[random[j ,1]]] <- random[j ,2]
  }
  
  #out$add_param$ICC <- var_effects / (var_effects + pi*pi/3)
  
  out$add_param$Residual <- pi*pi/3
  out$add_param$Observations <- length(model@resp$y)
  
  performance <- performance::model_performance(model)
  
  out$add_param$ICC <- performance$ICC
  out$add_param$"R\u00b2 Conditional" <-  performance$"R2_conditional"
  out$add_param$"R\u00b2 Marginal" <-  performance$"R2_marginal"

  random_vars <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)
  n_random <- lapply(
    random_vars, 
    \(x) model@frame[[x]] |> unique() |> length()
  ) |> unlist()
  for(i in seq_along(n_random)) {
    out$add_param[[paste0("N ", random_vars[i])]] <- n_random[i]
  }
  
  out
}

#' @export
extract_model_param.lmerMod <- function(model, ...) {
  message("Converted lmerMod object to lmerModLmerTest.")
  extract_model_param(lmerTest::as_lmerModLmerTest(model))
}


#' @export
extract_model_param.gls <- function(model, or = FALSE, ...) {
  
  model_summary <- summary(model)
  
  out <- list()
  
  out$auto_labels <- get_labels(model$model)
  out$labels_models <- model$terms[[2]] |> as.character()
  
  out$add_param$Observations <- model$dims$N
  out$add_param[["R\u00b2"]] <- performance::r2(model)$R2
  #out$add_param[["R\u00b2 adjusted"]] <- model_summary$adj.r.squared
  out$add_param[["Residual"]] <- model_summary$sigma^2
  out$add_param[["AIC"]] <- model_summary$AIC
  out$add_param[["BIC"]] <- model_summary$BIC
  #if (inherits(model, "glm")) out$add_param$"R\u00b2 Tjur" <-  performance::r2_tjur(model)
  
  out$estimates$fixed <- as.data.frame(coef(model_summary))
  out$estimates$p_label <- names(out$estimates$fixed)[ncol(out$estimates$fixed)]
  
  if (or) {
    out$estimates$fixed[[1]] <- exp(out$estimates$fixed[[1]])
  }  
  
  out
}
