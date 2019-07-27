
nice_stats <- function(stat, level = TRUE) {
  lev <- function(p) {
    if (!level) {
      return(round(p, 3))
    }
    if (p > .05) out <- paste0("> ", trunc(p * 100) / 100)
    if (p <= .05) out <- "< .05"
    if (p <= .01) out <- "< .01"
    if (p <= .001) out <- "< .001"
    out
  }
  type <- class(stat)
  out <- NA
  if (type[1] == "aov" & type[2] == "lm") {
    stat <- summary(stat)[[1]]
    p <- lev(stat$"Pr(>F)"[1])
    out <- sprintf("F(%d, %d) = %.2f; p %s", stat$Df[1], stat$Df[2], stat$"F value"[1], p)
  }
  
  if (type[1] == "htest") {
    p <- lev(stat$p.value)
    if (names(stat$statistic) == "t") {
      out <- sprintf("t(%.1f) = %.2f; p %s", stat$parameter, stat$statistic, p)
    }
    if (names(stat$statistic) == "X-squared") {
      out <- sprintf("X-Squared(%d) = %.2f; p %s", stat$parameter, stat$statistic, p)
    }
  }
  out
}

