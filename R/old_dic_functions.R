################# FUNCTIONS FOR DICTIONARY FILES ############

nameItemsDIC <- function(data, dic, scales = NULL) {
  names.data <- names(data)
  for (i in 1:length(dic$Code)) {
    no <- which(names.data == dic$Code[i])
    if (isFALSE(no > 0)) {
      print(dic$Code[i], " not found\n")
    }
    names(data)[no] <- as.character(dic$INDEX_ITEM[i])
  }
  
  if (!is.null(scales)) {
    for (VAR in 1:length(scales)) {
      data[, names(scales)[VAR]] <- getscale(data, dic, scales[[VAR]])$values
    }
  }
  
  data
}

getscaleDIC <- function(data, dic, scales) {
  var.names <- list()
  values <- list()
  keys <- list()
  for (i in 1:length(scales)) {
    scale <- scales[i]
    VAR <- dic$INDEX_ITEM[dic$SKALA %in% scale]
    
    if (length(VAR) == 0) stop(scale, " not found.\n")
    KEY <- dic$KEY[dic$INDEX_ITEM %in% VAR]
    score <- scoreFast(keys = matrix(KEY, ncol = 1, dimnames = list(VAR, "scale")), items = data[, VAR], delete = FALSE)
    values[[i]] <- as.numeric(score)
    var.names[[i]] <- VAR
    keys[[i]] <- KEY
    names(values)[i] <- names(scales)[i]
    names(keys)[i] <- names(scales)[i]
    names(var.names)[i] <- names(scales)[i]
  }
  out <- list(items = var.names, keys = keys, values = as.data.frame(values))
  
  out
}

alphascaleDIC <- function(data, dic, scales, ...) {
  var.names <- list()
  values <- list()
  keys <- list()
  for (i in 1:length(scales)) {
    scale <- scales[i]
    VAR <- dic$INDEX_ITEM[dic$SKALA %in% scale]
    
    if (length(VAR) == 0) stop(scale, " not found.\n")
    KEY <- dic$KEY[dic$INDEX_ITEM %in% VAR]
    
    var.names[[i]] <- VAR
    keys[[i]] <- matrix(KEY, ncol = 1, dimnames = list(VAR, "scale"))
    names(keys)[i] <- names(scales)[i]
    names(var.names)[i] <- names(scales)[i]
  }
  
  alpha.table(data, var.names, labels = names(scales), check.key = FALSE, key = keys, ...)
}

checkValuesDIC <- function(data, dic) {
  errors <- list()
  for (i in 1:nrow(dic)) {
    if (DIC$VALUES[i] != "char") {
      values <- eval(parse(text = paste0("c(", dic$VALUES[i], ")")))
      nr <- which(!(data[, dic$INDEX_ITEM[i]] %in% values) & !is.na(data[, dic$INDEX_ITEM[i]]))
      if (length(nr) == 0) {
        errors[[i]] <- "clean"
      } else {
        errors[[i]] <- paste0("case ", nr, ": ", data[nr, dic$INDEX_ITEM[i]])
      }
    } else {
      errors[[i]] <- "char"
    }
  }
  names(errors) <- dic$INDEX_ITEM
  cat("Invalid values:\n")
  for (i in 1:length(errors)) {
    if (!(errors[[i]][1] %in% c("clean", "char"))) {
      cat(paste0("Variable: ", names(errors)[i]), errors[[i]], sep = "\n")
    }
  }
  invisible(errors)
}

scalebookDIC <- function(data, dic, scale, round = 2, ...) {
  dic <- dic[dic$SKALA %in% scale, ]
  VAR <- dic$INDEX_ITEM
  
  if (length(VAR) == 0) stop(scale, " not found.\n")
  keys <- dic$KEY
  keys <- matrix(keys, ncol = 1, dimnames = list(VAR, "scale"))
  
  a <- alpha(data[, VAR], keys = keys)
  d <- round(describe(data[, VAR]), round)
  df <- data.frame(Item = VAR, Key = as.numeric(keys))
  df <- cbind(df, d[c("n", "min", "max", "mean", "sd", "median", "mad", "skew", "kurtosis")])
  df <- cbind(df, round(a$item.stats[c("r.drop")], round))
  df <- cbind(df, round(a$alpha.drop[c("raw_alpha")], round))
  
  df.alpha <- alphascaleDIC(data, dic, scale)
  df.items <- data.frame(Short = dic$INDEX_ITEM, long = dic$ITEM, Key = dic$KEY)
  
  row.names(df) <- NULL
  row.names(df.alpha) <- NULL
  row.names(df.items) <- NULL
  out <- list(itemanalyzes = df, alpha = df.alpha, items = df.items)
  out
}
