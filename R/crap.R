
### SPSS files

#spss_date2R <- function(x) {
#  out <- as.Date(x / (24 * 60 * 60), origin = "1582-01-01")
#  out
#}




### comparing and testing a random slope effect against a model without random slope effect

# random_slope <- function(dat, DV, grouping, IV) {
#   out <- list()
#   VAR <- c(DV, grouping, IV)
#   dat <- dat[, VAR]
#   dat <- dat[complete.cases(dat), ]
#   dv <- dat[, DV]
#   iv <- dat[, IV]
#   level2 <- dat[, grouping]
# 
#   model.a <- lme(dv ~ iv, random = ~ iv | level2, control = list(opt = "optim"))
#   model.b <- update(model.a, random = ~ 1 | level2)
#   out$model.a <- model.a
#   out$model.b <- model.b
#   out$anova <- anova(model.a, model.b)
#   out$variables <- list(dependent = DV, independent = IV, level2 = grouping)
#   out
# }


#####
# sociometric_position <- function(measures, grouping, pair = NA, concise = TRUE, stat.cat = c("Popular", "Rejected", "Neglected", "Controversial", "Average")) {
#   new.dat <- data.frame(order = 1:length(grouping), grouping, measures)
#   names.measures <- names(measures)
#   measures <- as.matrix(measures)
#   t.m <- aggregate(measures ~ grouping, FUN = mean, na.rm = TRUE)
#   colnames(t.m) <- c("grouping", paste0(names.measures, ".mean"))
#   t.s <- aggregate(measures ~ grouping, FUN = sd, na.rm = TRUE)
#   colnames(t.s) <- c("grouping", paste0(names.measures, ".sd"))
#   t.n <- as.data.frame(table(grouping))
#   names(t.n) <- c("grouping", "n.group")
#   new.dat <- merge(new.dat, t.m, by = "grouping", all.x = TRUE)
#   new.dat <- merge(new.dat, t.s, by = "grouping", all.x = TRUE)
#   new.dat <- merge(new.dat, t.n, by = "grouping", all.x = TRUE)
# 
#   for (i in 1:length(names.measures)) {
#     new.dat[, paste0("z_", names.measures[i])] <- (new.dat[, names.measures[i]] - new.dat[, paste0(names.measures[i], ".mean")]) / new.dat[, paste0(names.measures[i], ".sd")]
#   }
#   for (i in 1:length(names.measures)) {
#     new.dat[, paste0("p_", names.measures[i])] <- new.dat[, names.measures[i]] / (new.dat$n.group - 1)
#   }
#   for (i in 1:length(names.measures)) {
#     new.dat[, paste0("petillon_", names.measures[i])] <- 1 + (new.dat[, names.measures[i]] - new.dat[, paste0(names.measures[i], ".mean")]) / (new.dat$n.group - 1)
#   }
#   for (i in 1:length(names.measures)) {
#     raw.by.group <- split(new.dat[, names.measures[i]], new.dat$grouping)
#     pr <- rep(NA, nrow(new.dat))
#     for (j in 1:nrow(new.dat)) {
#       y <- raw.by.group[[new.dat$grouping[j]]]
#       pr[j] <- sum(new.dat[j, names.measures[i]] > y) / length(y)
#     }
#     new.dat[, paste0("pr_", names.measures[i])] <- pr
#   }
#   for (i in 1:length(names.measures)) {
#     new.dat[, paste0("petillon_cat_", names.measures[i])] <- cut(new.dat[, paste0("petillon_", names.measures[i])], c(0, 0.8, 1.2, Inf), labels = c("low", "middle", "high"), include = TRUE, ordered = TRUE)
#   }
# 
#   if (length(pair) != 1) {
# 
#     ### based open Z-values
# 
#     new.dat$IST.Z <- new.dat[, paste0("z_", pair[1])] - new.dat[, paste0("z_", pair[2])]
#     t.m <- aggregate(new.dat$IST.Z ~ grouping, FUN = mean, na.rm = TRUE)
#     colnames(t.m)[2] <- "IST.Z.mean"
#     t.s <- aggregate(new.dat$IST.Z ~ grouping, FUN = sd, na.rm = TRUE)
#     colnames(t.s)[2] <- "IST.Z.sd"
#     new.dat <- merge(new.dat, t.m, by = "grouping", all.x = TRUE)
#     new.dat <- merge(new.dat, t.s, by = "grouping", all.x = TRUE)
#     new.dat$IST.Z <- (new.dat$IST.Z - new.dat$IST.Z.mean) / new.dat$IST.Z.sd
# 
#     new.dat$impact.Z <- new.dat[, paste0("z_", pair[1])] + new.dat[, paste0("z_", pair[2])]
#     t.m <- aggregate(new.dat$impact.Z ~ grouping, FUN = mean, na.rm = TRUE)
#     colnames(t.m)[2] <- "impact.Z.mean"
#     t.s <- aggregate(new.dat$impact.Z ~ grouping, FUN = sd, na.rm = TRUE)
#     colnames(t.s)[2] <- "impact.Z.sd"
#     new.dat <- merge(new.dat, t.m, by = "grouping", all.x = TRUE)
#     new.dat <- merge(new.dat, t.s, by = "grouping", all.x = TRUE)
#     new.dat$impact.Z <- (new.dat$impact.Z - new.dat$impact.Z.mean) / new.dat$impact.Z.sd
# 
#     ### based on Petillon algorithm
# 
#     new.dat$IST.petillon <- new.dat[, paste0("petillon_", pair[1])] - new.dat[, paste0("petillon_", pair[2])]
#     t.m <- aggregate(new.dat$IST.petillon ~ grouping, FUN = mean, na.rm = TRUE)
#     colnames(t.m)[2] <- "IST.petillon.mean"
#     t.s <- aggregate(new.dat$IST.petillon ~ grouping, FUN = sd, na.rm = TRUE)
#     colnames(t.s)[2] <- "IST.petillon.sd"
#     new.dat <- merge(new.dat, t.m, by = "grouping", all.x = TRUE)
#     new.dat <- merge(new.dat, t.s, by = "grouping", all.x = TRUE)
#     new.dat$IST.petillon <- (new.dat$IST.petillon - new.dat$IST.petillon.mean) / new.dat$IST.petillon.sd
# 
#     ####
#     new.dat$impact.petillon <- new.dat[, paste0("petillon_", pair[1])] + new.dat[, paste0("petillon_", pair[2])]
#     t.m <- aggregate(new.dat$impact.petillon ~ grouping, FUN = mean, na.rm = TRUE)
#     colnames(t.m)[2] <- "impact.petillon.mean"
#     t.s <- aggregate(new.dat$impact.petillon ~ grouping, FUN = sd, na.rm = TRUE)
#     colnames(t.s)[2] <- "impact.petillon.sd"
#     new.dat <- merge(new.dat, t.m, by = "grouping", all.x = TRUE)
#     new.dat <- merge(new.dat, t.s, by = "grouping", all.x = TRUE)
#     new.dat$impact.petillon <- (new.dat$impact.petillon - new.dat$impact.petillon.mean) / new.dat$impact.petillon.sd
# 
#     status.groups <- function(IST, impact, WST, AST) {
#       out <- rep(NA, length(IST))
#       out[which(!is.na(impact) & !is.na(IST))] <- stat.cat[5]
#       out[which(IST > 1 & WST > 0 & AST < 0)] <- stat.cat[1]
#       out[which(IST < -1 & WST < 0 & AST > 0)] <- stat.cat[2]
#       out[which(impact < -1 & WST < 0 & AST < 0)] <- stat.cat[3]
#       out[which(impact > 1 & WST > 0 & AST > 0)] <- stat.cat[4]
#       out
#     }
# 
#     WST <- new.dat[, paste0("z_", pair[1])]
#     AST <- new.dat[, paste0("z_", pair[2])]
#     IST <- new.dat$IST.Z
#     impact <- new.dat$impact.Z
#     new.dat$Status.Group <- status.groups(IST, impact, WST, AST)
# 
#     WST <- new.dat[, paste0("petillon_", pair[1])]
#     AST <- new.dat[, paste0("petillon_", pair[2])]
#     IST <- new.dat$IST.petillon
#     impact <- new.dat$impact.petillon
#     new.dat$Status.Group.petillon <- status.groups(IST, impact, WST, AST)
# 
# 
#     # new.dat$IST.Petillon <- new.dat[,paste0("petillon_",pair[1])] -  new.dat[,paste0("petillon_",pair[2])]
# 
# 
#     if (concise) {
#       new.dat <- new.dat[, !names(new.dat) %in% c("IST.Z.mean", "IST.Z.sd", "impact.Z.mean", "impact.Z.sd")]
#     }
#   }
#   new.dat <- new.dat[order(new.dat$order), ]
#   new.dat <- new.dat[, !names(new.dat) == "order"]
# 
#   if (concise) {
#     new.dat <- new.dat[-(1:(length(names.measures) * 3 + 1))]
#   }
#   # else
#   # new.dat <- new.dat[-(1:(length(names.measures)+1))]
# 
#   new.dat
# }


# read.clip <- function(sep = "\t", header = TRUE) read.table(file = "clipboard", sep = sep, header = header)
# write.clip <- function(dat, sep = "\t", col.names = TRUE) write.table(dat,file = "clipboard", sep = sep, col.names = col.names)

