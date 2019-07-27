#' Table with alpha values
#'
#' Returns a list of alpha cronbachs. VAR is a list of character strings
#' @param data A data Frame
#' @param VAR A list containing vectors with variable names. Each list element defines one scale.
#' @param labels Label names for scales
#' @param round Rounds values to given decimal position
#' @param CI If TRUE confidence intervals are calculated
#' @param conf_level Confidence level (e.g. 0.95 for 95%)
#' @param check_key Check_key for the psych::alpha function
#' @param omega If TRUE Omega reliability estimation is calculated
#' @param key Optional key argument for the psych::alpha function
#' @param RMSEA If TRUE RMSEA is calculated
#' @export
alpha_table <- function(data, VAR, labels = NULL, round = 2, CI = TRUE, conf_level = 0.95, check_key = TRUE, omega = FALSE, key = NULL, RMSEA = FALSE) {
  if (is.null(labels)) labels <- labels(VAR)
  df <- data.frame(Scale = labels)
  for (i in 1:length(VAR)) {
    if (!is.null(key)) {
      KEY <- key[[i]]
    } else {
      KEY <- NULL
    }
    
    a <- invisible(psych::alpha(data[, VAR[[i]]], check.key = check_key, keys = KEY))
    if (omega) {
      o <- invisible(psych::omega(data[, VAR[[i]]], nfactors = 1, keys = KEY))
    }
    f <- invisible(psych::fa(data[, VAR[[i]]]))
    alpha <- a$total$raw_alpha
    df$"n"[i] <- min(a$item.stats$n, na.rm = TRUE)
    
    df$"n items"[i] <- a$nvar
    
    if (!CI) {
      df$Alpha[i] <- substring(round(alpha, round), 2)
    }
    
    if (CI) {
      a.CI <- alpha_CI(alpha, nrow(data[, VAR[[i]]]), length(VAR[[i]]), conf_level)
      df$Alpha[i] <- paste0(substring(round(alpha, round), 2), " (", substring(round(a.CI[1], round), 2), "-", substring(round(a.CI[2], round), 2), ")")
    }
    
    alpha.std <- a$total$std.alpha
    if (!CI) {
      df$"Std.Alpha"[i] <- substring(round(alpha.std, round), 2)
    }
    
    if (CI) {
      a.std.CI <- alpha_CI(alpha.std, nrow(data[, VAR[[i]]]), length(VAR[[i]]), conf_level)
      df$"Std.Alpha"[i] <- paste0(
          substring(round(alpha.std, round), 2), 
          " (", 
          substring(round(a.std.CI[1], round), 2), 
          "-", 
          substring(round(a.std.CI[2], round), 2), 
          ")"
      )
    }
    
    df$"Homogeneity"[i] <- substring(round(a$total$average_r, round), 2)
    dmin <- round(min(a$item.stats$r.drop), round)
    dmax <- round(max(a$item.stats$r.drop), round)
    df$"Discrimination"[i] <- paste0(substring(dmin, 2), " - ", substring(dmax, 2))
    mmin <- round(min(a$item.stats$mean), round)
    mmax <- round(max(a$item.stats$mean), round)
    df$"M"[i] <- paste0(mmin, " - ", mmax)
    smin <- round(min(a$item.stats$sd), round)
    smax <- round(max(a$item.stats$sd), round)
    df$"SD"[i] <- paste0(smin, " - ", smax)
    if (omega) {
      df$"Omega"[i] <- substring(round(o$omega.tot, round), 2)
    }
    
    lmin <- round(min(abs(f$loadings)), round)
    lmax <- round(max(abs(f$loadings)), round)
    df$"|Loading|"[i] <- paste0(substring(lmin, 2), " - ", substring(lmax, 2))
    if (RMSEA) {
      df$"RMSEA"[i] <- substring(round(f$RMSEA[1], 3), 2)
    }
  }
  
  if (CI) {
    names(df)[which(names(df) == "Alpha")] <- paste0("Alpha(CI", conf_level * 100, "%)")
    names(df)[which(names(df) == "Std.Alpha")] <- paste0("Std.Alpha(CI", conf_level * 100, "%)")
  }
  
  return(df)
}

