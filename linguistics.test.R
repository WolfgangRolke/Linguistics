linguistics.test <- function(x, y, alpha=0.05) {
   if(!is.numeric(x)) {cat("Data has to be numeric\n");return(NULL)}
   if(!missing(y)) x=cbind(x, y)
   p=chisq.test(x)$p.value
   if(p>alpha) {
     print(paste0("Test fail to reject the null hypothesis at the ", 100*alpha,"% level\n"))
     return(round(p, 4))
   }
   print(paste0("Test rejects the null hypothesis at the ", 100*alpha,"% level\n"))
   m <- dim(x)[1]
   n <- x[, 2]+x[, 1]
   if(is.null(rownames(x))) names(n) <- 1:m
   else names(n) <- rownames(x)
   x <- x[, 1]
   names(x) <- names(n)
   z <- pairwise.prop.test(x, n, p.adjust.method="holm")$p.value
   out <- rep(0, (m-1)^2)
   k <- 0
   for(i in 1:(m-1))
     for(j in 1:(m-1)) {
       k <- k+1
       names(out)[k] <- paste(rownames(z)[i], " - ",
                              colnames(z)[j])
       out[k] <- z[i, j]
     }
   out <- out[!is.na(out)]
   out <- out[order(out)]
   list(p.value.test=round(p, 4), p.value.multiple.comparisson=round(out, 4))
}