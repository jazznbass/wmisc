#' Perform within agreement analysis
#'
#' This function performs an agreement analysis including intra-class
#' correlation coefficients, group-related variance, and related diagnostics for
#' items with a nominal or ordinal scale. The function outputs a data.frame or a
#' HTML table giving the results.
#'
#' @param data A data-frame
#' @param vars Vector of character strings with names of variables to be
#'   analyzed.
#' @param grouping Grouping variable. Either as a character string or a vector.
#' @param rv Number of response options for calculating random variance.
#' @param crit Critical agreement level.
#' @param min_group_size Minimal group size. Smaller groups are excluded.
#' @param labels Character vector with new variable labels.
#' @param auto_labels If TRUE, variable names are taken from a label attribute.
#' @param n_sim Number of simulations for the agreement analyses.
#'
#' @return A Data-frame
#'
#' @examples
#'
#' vars <- c("Norm_Pop_academic", "Norm_Pop_conform", "Norm_Pop_sporty",
#'   "Norm_Pop_cool_clothes", "Norm_Pop_helpful", "Norm_Unpop_academic",
#'   "Norm_Unpop_nonconform", "Norm_Unpop_sporty", "Norm_Unpop_cool_clothes",
#'   "Norm_Unpop_language", "Norm_LM_academic", "Norm_LM_conform",
#'   "Norm_LM_sporty", "Norm_LM_cool_clothes", "Norm_LM_helpful",
#'   "Norm_LL_academic", "Norm_LL_nonconform", "Norm_LL_sporty", "Norm_LL_cool_clothes",
#'   "Norm_LL_language")
#'
#' nice_agreement_table(
#'     data = wmisc:::ex_agreement,
#'     vars = vars,
#'     grouping = "id_class_teacher",
#'     rv = 4,
#'     crit = 0.6,
#'     n_sim = 100)
#' @export
nice_agreement_table <- function(data, 
                                 vars = names(data), 
                                 grouping, 
                                 rv, 
                                 crit = 0.7, 
                                 min_group_size = 0,
                                 auto_labels = TRUE,
                                 labels = NULL, 
                                 n_sim = 10000) {
  
  out <- do.call(agreement_analysis, as.list(environment()))
  nice_table(out)
}

#' @export
#' @rdname nice_agreement_table
agreement_analysis <- function(data, 
                               vars = names(data), 
                               grouping, 
                               rv, 
                               crit = 0.7, 
                               min_group_size = 0,
                               auto_labels = TRUE,
                               labels = NULL, 
                               n_sim = 10000) {
  
  if (inherits(grouping, "character")) vars <- vars[!vars %in% grouping]
  
  if (auto_labels) labels <- get_labels(data[vars])
  if (is.null(labels)) labels <- names(data[vars])
  
  r_wg <- c()
  r_wg_min <- c()
  r_wg_max <- c()
  r_wg_95 <- c()
  r_wg_p <- c() # proportion of rwg > 95% ci
  r_wg_crit <- c()
  icc1 <- c()
  icc2 <- c()
  p_icc <- c()
  L_icc <- c()
  G_rel <- c()
  G_var <- c()
  n <- c()
  n_classes <- c()
  n_min <- c()
  n_max <- c()

  ranvar <- function(a) (a^2 - 1) / 12 
  # a = Number of response options (Bliese 2013)

  if (inherits(grouping, "character")) {
    group_total <- as.factor(data[[grouping]])
  } else {
    group_total <- as.factor(grouping)
  }

  for (i in 1:length(vars)) {
   
    x <- data.frame(
      x = data[[vars[i]]],
      group = group_total
    )
    
    x <- x[!is.na(x$x),]
    
    group_sizes <- table(x$group)
    id_valid <- names(group_sizes)[group_sizes >= min_group_size]
    id_valid <- which(x$group %in% id_valid)
    x <- x[id_valid,]
    x$group <- droplevels(x$group)
    
    group_sizes <- table(x$group)
    mean_gsize <- group_sizes |> mean(na.rm = TRUE) |> round()
    n_min[i] <- min(group_sizes, na.rm = TRUE) 
    n_max[i] <- max(group_sizes, na.rm = TRUE) 
    
    #if (mean_gsize == 0) mean_gsize <- 1
    n[i] <- nrow(x)
    
    agreement <- multilevel::rwg(x$x, x$group, ranvar = ranvar(rv))
    r_wg[i] <- mean(agreement$rwg, na.rm = TRUE)
    r_wg_min[i] <- min(agreement$rwg, na.rm = TRUE)
    r_wg_max[i] <- max(agreement$rwg, na.rm = TRUE)    
    n_classes[i] <- sum(!is.na(agreement$rwg))
    r_wg_crit[i] <- mean(agreement$rwg >= crit, na.rm = TRUE)
    r_wg95 <- multilevel::rwg.sim(
      gsize = mean_gsize, nresp = rv, nrep = n_sim
    )$rwg.95
 
    if (length(r_wg95) == 0) {
      r_wg_95[i] <- NA
    } else {
      r_wg_95[i] <- r_wg95
    }

    r_wg_p[i] <- mean(agreement$rwg > r_wg_95[i], na.rm = TRUE)
    if (length(r_wg95) == 0) r_wg_p[i] <- NA
    
    fit <- aov(x ~ group, data = x)
    icc1[i] <- multilevel::ICC1(fit)
    icc2[i] <- multilevel::ICC2(fit)
    null_model <- lme(x ~ 1, random = ~ 1 | group, data = x, method = "ML")
    model_without <- gls(x ~ 1, data = x, method = "ML")
    dif <- anova(null_model, model_without)
    L_icc[i] <- dif$L.Ratio[2]
    p_icc[i] <- dif$"p-value"[2]
    G_rel[i] <- mean(multilevel::gmeanrel(null_model)$MeanRel, na.rm = TRUE)
    v <- VarCorr(null_model)
    #G_var[i] <- as.numeric(v[[1]][1]) / 
    #            (as.numeric(v[[1]][1]) + as.numeric(v[[2]][1]))
  }
  
  out <- data.frame(
    Label = labels,
    "Ratings" = n, 
    "Classes" = n_classes, 
    "Min" = n_min,
    "Max" = n_max,
    "Min " = round(r_wg_min, 2),
    "Max " = round(r_wg_max, 2),
    "Mean" = round(r_wg, 2),
    "Rwg >= crit" = paste0(round(r_wg_crit * 100, 1), "%"), 
    "CI" = round(r_wg_95, 2),
    "> CI" = paste0(round(r_wg_p * 100, 1), "%"), 
    ICC = round(icc1, 2),
    "L icc" = round(L_icc, 1), 
    "p icc" = round(p_icc, 3),
    "ICC(2)" = round(icc2, 2), 
    #"Group mean reliability" = round(G_rel, 2), 
    #"Group explained variance" = round(G_var, 2),
    check.names = FALSE
  )
  
  names(out)[9] <- paste0("\u2265 ", crit)

  note <- c(
    "*Rwg* is the within group agreement", 
    "*CI* is the upper bound of a 95% inteval of rwg", 
    "*ICC(1)* is the intra-class correlation (i.e. the amount of individual-level variance that can be explained by group membership)", 
    "*L* is the likelihood ratio", 
    "*ICC(2)* is the reliability of the group means"
  )

  if (!is.null(min_group_size)) note <- c(
    note, 
    paste0("Groups with less than ", 
            min_group_size, 
           " cases were removed from the analysis")
  )  
  rownames(out) <- NULL
  out <- set_wmisc_attributes(
    out, 
    spanner = list("N" = 2:3, "Class size" = 4:5, "Rwg" = 6:11, "ICC(1)" = 12:14),
    title = "Within group agreement analysis",
    note = note
  )

  out
}



