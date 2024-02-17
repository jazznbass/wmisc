#' Perform agreement analysis
#'
#' This function performs an agreement analysis including intraclass correlation
#' coefficients, group-related variance, and related diagnostics for items with
#' a nominal or ordinal scale. The function outputs a data.frame or a HTML table
#' giving the results.
#'
#' @param data A data-frame
#' @param vars Vector of character strings with names of variables to be
#'   analyzed
#' @param grouping Grouping variable. Either as a character string or a vector
#' @param rv Number of response options for calculating random variance
#' @param crit Critical agreement level
#' @param label Character vector with new variable labels
#' @param n_sim Number of simulations for the agreement analyses
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
#'   "Norm_LL_language", "Unpop_in", "Pop_in", "LL_in", "LM_in", "Unpop_out", 
#'   "Pop_out", "LL_out", "LM_out")
#'   
#' nice_agreement_table(
#'     data = wmisc:::ex_agreement, 
#'     vars = vars, grouping = "id_class_teacher", 
#'     rv = 4, 
#'     crit = 0.6, 
#'     n_sim = 100)
#' @export
agreement_analysis <- function(data, 
                               vars = names(data), 
                               grouping, 
                               rv, 
                               crit = 0.7, 
                               type = "df", 
                               label, 
                               n_sim = 10000) {
  
  if (inherits(grouping, "character")) vars <- vars[!vars %in% grouping]
  if (missing(label)) label <- names(data[vars])

  if (identical(label, "haven")) {
    label <- mapply(
      function(.x, .y) if(!is.null(attr(.x, "label"))) attr(.x, "label") else .y,
      .x = data[vars], 
      .y = names(data[vars])
    )    
  }
  
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

  ranvar <- function(a) (a^2 - 1) / 12 
  # a = Number of response options (Bliese 2013)

  if (inherits(grouping, "character")) {
    group_total <- as.factor(data[[grouping]])
  } else {
    group_total <- as.factor(grouping)
  }

  for (i in 1:length(vars)) {
    valid <- !is.na(data[[vars[i]]])
    x <- data[[vars[i]]][valid]
    group <- group_total[valid]
  
    mean_gsize <- 
      group  |> 
      table()  |> 
      mean(na.rm = TRUE)  |> 
      round()
    
    if (mean_gsize == 0) mean_gsize <- 1
    n[i] <- length(x)
    
    agreement <- multilevel::rwg(x, group, ranvar = ranvar(rv))
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
    
    fit <- aov(x ~ group)
    icc1[i] <- multilevel::ICC1(fit)
    icc2[i] <- multilevel::ICC2(fit)
    null.model <- lme(x ~ 1, random = ~ 1 | group, method = "ML")
    model.without <- gls(x ~ 1, method = "ML")
    dif <- anova(null.model, model.without)
    L_icc[i] <- dif$L.Ratio[2]
    p_icc[i] <- dif$"p-value"[2]
    G_rel[i] <- mean(multilevel::gmeanrel(null.model)$MeanRel, na.rm = TRUE)
    v <- VarCorr(null.model)
    #G_var[i] <- as.numeric(v[[1]][1]) / 
    #            (as.numeric(v[[1]][1]) + as.numeric(v[[2]][1]))
  }
  
  out <- tibble(
    #Var = names(data[vars]), 
    Label = label,
    n = n, 
    "N l2" = n_classes, 
    "Min rwg" = round(r_wg_min, 2),
    "Max rwg" = round(r_wg_max, 2),
    "Mean rwg" = round(r_wg, 2),
    "Rwg >= crit" = paste0(round(r_wg_crit * 100, 1), "%"), 
    "Rwg upper 95% CI" = round(r_wg_95, 2),
    "Proportion > 95%CI" = round(r_wg_p, 2), 
    ICC = round(icc1, 2),
    "L icc" = round(L_icc, 1), 
    "p icc" = round(p_icc, 3),
    "ICC(2)" = round(icc2, 2), 
    "Group mean reliability" = round(G_rel, 2)#, 
    #"Group explained variance" = round(G_var, 2)
  )

  out <- set_wmisc_attributes(out, title = "Agreement analysis")
  
  if (type == "df") return(out)
  if (type == "html") {
    return(nice_table(out))
  }

  out
}

#' @export
#' @rdname agreement_analysis
nice_agreement_table <- function(..., type = "html") {
  agreement_analysis(..., type = type)
}
