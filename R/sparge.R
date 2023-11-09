
plot.sparge <- function(x, jit.f=1, zl=TRUE, xlim=range(unlist(unlist(x))), add=FALSE, pd=0, box.brdrs='gray', col=1, alpha=.3, ...){

   if(length(col)==1){ col <- rep(col,length(x))}
   # convert to transparent colors
   cols <- sapply(col, function(cl){ RGB <- col2rgb(cl)/255; rgb(RGB[1,],RGB[2,],RGB[3,], alpha=alpha)})

   q <- length(x)
   for(yi in 1:q){
    xs <- x[[yi]]
    ys <- jitter(rep(yi,length(xs)), factor=jit.f, amount=1/10) + pd
   
    if(yi==1 & !add)
     plot(xs, ys, ylim=c(1,q), xlim=xlim, ...)   #pch=21  bg= ...
    
     points(xs, ys, pch=21, bg=cols[yi], ...)
     if(!is.null(box.brdrs))
      boxplot(x, range=0, yaxt='n', xaxt='n', horizontal=T, col='transparent', varwidth=T, las=1, add=TRUE, border=box.brdrs, boxwex=.2)
    }
   
  #}
 if(zl){abline(v=0, lty=2)}
}
