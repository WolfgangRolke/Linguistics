do.anova <- function(y, n=85, k=6, j=4) {
  groups <- rep(LETTERS[1:j], each=k)
  resp <- matrix(0, n, j)
  colnames(resp) <- LETTERS[1:j]
  for(i in 1:j) {
    tmp <- y[, groups==LETTERS[i]] 
    resp[, i] <- apply(tmp, 1, mean, na.rm=TRUE)
  }
  df <- data.frame(resp=c(resp), 
                   group=rep(groups, each=n))
  fit <- aov(df$resp ~ df$group)
  as.numeric(unlist(summary(fit))[9])
}

gen.data.anova <- function(p=c(0.78, 0.58, 0.78, 0.84), 
           n=85, k=6, j=4, n.missing) {
    groups <- rep(LETTERS[1:j], each=k)
    y <- matrix(0, n, k*j)
    colnames(y) <- groups
    for(i in 1:j) {
      y[ ,colnames(y)==LETTERS[i]] <- 
        sample(0:1, size = n*k, replace = TRUE, 
               prob = c(1-p[i], p[i]))
    }
    if(!missing(n.missing)) {
      for(i in 1:n.missing) {
        y[sample(1:n, 1), sample(1:(k*j), 1)] <- NA
      }
    }
    y
}

