
############### psych package extensions

# takes a psych fa object and gives labels of items of factors
comp2l <- function(comp, cut = 0.4) {
  nr <- length(attr(comp$loadings, "dimnames")[[1]])
  nc <- length(attr(comp$loadings, "dimnames")[[2]])
  lo <- as.data.frame(matrix(as.numeric(comp$loadings), nrow = nr, ncol = nc))
  row.names(lo) <- attr(comp$loadings, "dimnames")[[1]]
  names(lo) <- attr(comp$loadings, "dimnames")[[2]]
  re <- vector("list", nc)
  for (i in 1:nc) {
    pc <- lo[order(abs(lo[i]), decreasing = TRUE), ]
    re[[i]]$label <- row.names(pc[abs(pc[i]) >= cut, ])
    re[[i]]$loading <- pc[abs(pc[i]) >= cut, i]
    # re[[i]] <- ifelse(pc[i] > 0, re[[i]], paste("- ",re[[i]], sep = ""))
  }
  re
}

### givs a data frame with loadings for a fa object
loadings.df <- function(comp) {
  lo <- comp$loadings
  lab <- attr(lo, "dimnames")[[1]]
  co <- attr(lo, "dimnames")[[2]]
  nr <- length(lab)
  nc <- length(co)
  out <- as.data.frame(matrix(as.numeric(lo), ncol = nc))
  row.names(out) <- lab
  names(out) <- co
  out[order(abs(out[, 1]), decreasing = TRUE), ]
}


fa.variance <- function(fa) {
  ev <- round(fa$values, 2)
  if (class(fa)[2] == "fa") {
    ev <- ev + 1
  }
  re <- data.frame(Eigenvalues = ev)
  ex.v <- ev[1:length(ev)] / sum(ev)
  re$percent.variance <- round(ex.v * 100)
  cum.ev <- ex.v[1]
  for (i in 2:length(ev)) {
    cum.ev <- c(cum.ev, cum.ev[i - 1] + ex.v[i])
  }
  re$Cum.percent.variance <- round(cum.ev * 100)
  re
}
