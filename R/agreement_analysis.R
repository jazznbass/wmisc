#' Agreement Analysis
#'
#' @param data A data-frame
#' @param vars Vector of character strings with names of vairables to be analyzed
#' @param grouping Grouping variable. Either as a character String or a vector
#' @param rv Number of response options for calculating random variance
#' @param crit Critical agreement level
#' @param label Character vector with new variable labels
#' @param n_sim Number of simulations for the agreement analyses
#'
#' @return A Data-frame
#' @export

agreement_analysis <- function(data, vars = names(data), grouping, rv, crit = 0.7, type = "df", label, n_sim = 10000) {
  
  if (class(grouping) == "character") vars <- vars[!vars %in% grouping]
  if (missing(label)) label <- names(data[vars])

  if (identical(label, "haven")) {
    label <- map2_chr(
      data[vars], 
      names(data[vars]), 
      ~ if(!is.null(attr(.x, "label"))) attr(.x, "label") else .y
    )    
  }
  
  r.wg <- c()
  r.wg_min <- c()
  r.wg_max <- c()
  r.wg.95 <- c()
  r.wg.p <- c() # anteil der rwg > 95% ci
  r.wg.crit <- c()
  icc1 <- c()
  icc2 <- c()
  p.icc <- c()
  L.icc <- c()
  G.rel <- c()
  G.var <- c()
  n <- c()
  n.classes <- c()

  ranvar <- function(a) (a^2 - 1) / 12 # A = Number of response options, nach Bliese 2013

  if (class(grouping) == "character") {
    group.total <- as.factor(data[[grouping]])
  } else {
    group.total <- as.factor(grouping)
  }

  for (i in 1:length(vars)) {
    valid <- !is.na(data[[vars[i]]])
    x <- data[[vars[i]]][valid]
    group <- group.total[valid]
    #mean.gsize <- round(mean(table(group), na.rm = TRUE))
    mean.gsize <- 
      group %>%
      table() %>%
      mean(na.rm = TRUE) %>%
      round()
    
    if (mean.gsize == 0) mean.gsize <- 1
    n[i] <- length(x)
    
    agreement <- multilevel::rwg(x, group, ranvar = ranvar(rv))
    r.wg[i] <- mean(agreement$rwg, na.rm = TRUE)
    r.wg_min[i] <- min(agreement$rwg, na.rm = TRUE)
    r.wg_max[i] <- max(agreement$rwg, na.rm = TRUE)    
    n.classes[i] <- sum(!is.na(agreement$rwg))
    r.wg.crit[i] <- mean(agreement$rwg >= crit, na.rm = TRUE)
    r.wg95 <- multilevel::rwg.sim(gsize = mean.gsize, nresp = rv, nrep = n_sim)$rwg.95
 
    if (length(r.wg95) == 0) {
      r.wg.95[i] <- NA
    } else {
      r.wg.95[i] <- r.wg95
    }

    r.wg.p[i] <- mean(agreement$rwg > r.wg.95[i], na.rm = TRUE)
    if (length(r.wg95) == 0) r.wg.p[i] <- NA
    
    fit <- aov(x ~ group)
    icc1[i] <- multilevel::ICC1(fit)
    icc2[i] <- multilevel::ICC2(fit)
    null.model <- lme(x ~ 1, random = ~ 1 | group, method = "ML")
    model.without <- gls(x ~ 1, method = "ML")
    dif <- anova(null.model, model.without)
    L.icc[i] <- dif$L.Ratio[2]
    p.icc[i] <- dif$"p-value"[2]
    G.rel[i] <- mean(multilevel::gmeanrel(null.model)$MeanRel, na.rm = TRUE)
    v <- VarCorr(null.model)
    G.var[i] <- as.numeric(v[[1]][1]) / (as.numeric(v[[1]][1]) + as.numeric(v[[2]][1]))
  }
  
  out <- tibble(
    Var = names(data[vars]), 
    label = label,
    n = n, 
    "n l2" = n.classes, 
    "min rwg" = round(r.wg_min, 2),
    "max rwg" = round(r.wg_max, 2),
    "mean rwg" = round(r.wg, 2),
    "rwg >= crit" = paste0(round(r.wg.crit * 100, 1), "%"), 
    "rwg upper 95% CI" = round(r.wg.95, 2),
    "Proportion > 95%CI" = round(r.wg.p, 2), 
    ICC = round(icc1, 2),
    L.icc = round(L.icc, 1), 
    p.icc = round(p.icc, 3),
    "ICC(2)" = round(icc2, 2), 
    M.G.Real = round(G.rel, 2), 
    G.Var = round(G.var, 2)
  )
  if (type == "df") return(out)
  if (type == "html") {
    out <- knitr::kable(out, caption = "Agreement analyses") %>%
      kableExtra::kable_classic_2()
    #out <- knitr::kable(out, caption = "Agreement analyses", align = c("l", rep("c", ncol(out) - 1)), row.names = FALSE) %>%
    #  kableExtra::kable_styling(bootstrap_options = "basic", full_width = FALSE) %>%
    #  kableExtra::column_spec(1, bold = TRUE, color = "black") %>%
    #  kableExtra::row_spec(1, hline_after = TRUE)
  }
  out
}

