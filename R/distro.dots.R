
distro.dots <- function(x, jit.f=1, add=FALSE, pd=0, vv=names(x), vvlabs=NULL, xlim=range(unlist(x)), ...){

   q <- length(vv)
   for(yi in 1:q){
    xs <- x[rev(vv)][[yi]]
    ys <- - jitter(rep(yi,length(xs)), factor=jit.f) + pd
    if(yi==1 & !add){
     plot(xs, ys, ylim=-c(1,q), ylab='', xlim=xlim, yaxt='n', ...) 
    }else{
     points(xs, ys, ...)
    }
   }
   if(!is.null(vvlabs)) vvlabs <- rev(vvlabs) else vvlabs <- rev(vv)
   axis(2, at=-1:-q, labels=vvlabs, las=1)

}

