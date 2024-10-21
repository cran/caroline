makeElipseCoords <- function(x0 = 0, y0 = 0, b = 1, a = 1, alpha = 0, pct.range = c(0,1), len = 50){
  rad.range <- 2 * pi * pct.range
  theta <- seq(rad.range[1], rad.range[2], length.out=len)
  x <- x0 + a * cos(theta) * cos(alpha) - b * sin(theta) * sin(alpha)
  y <- y0 + a * cos(theta) * sin(alpha) + b * sin(theta) * cos(alpha)
  return(tab2df(cbind(x,y)))
}


plotClock <- function(hour, minute, x0 = 0, y0 = 0, r = 1){  
  
  circleXY <- makeElipseCoords(x0 = x0, y0 = y0, b = 1.1*r, a = 1.1*r, alpha = 0, 
                               pct.range = c(0,1), len = 50)
  quarHourTickMarksXY <- makeElipseCoords(x0 = x0, y0 = y0, b = 1.05*r, a = 1.05*r, alpha = (pi/2), 
                               pct.range = c((12*4-1)/(12*4),0), len = 12*4)
  hourLabelsXY <- makeElipseCoords(x0 = x0, y0 = y0, b = .9*r, a = .9*r, alpha = (pi/2), 
                               pct.range = c(11/12,0), len = 12)

  polygon(circleXY)
  text(hourLabelsXY[,1],hourLabelsXY[,2],seq(1,12), cex=.5)
  text(quarHourTickMarksXY[,1],quarHourTickMarksXY[,2],".")

  minuteV <- minute/60
  minuteVXY <- makeElipseCoords(x0 = x0, y0 = y0, b = r, a = r, alpha = 0, 
  				               pct.range =  (.25 - rep(minuteV,2)), len = 1)
  segments(x0,y0,minuteVXY$x[1],minuteVXY$y[1])

  hourV <- hour/12
  hourVXY <- makeElipseCoords(x0 = x0, y0 = y0, b = .7*r, a =.7*r, alpha = 0, 
  				               pct.range = (.25 - rep(hourV,2)), len = 1)
  segments(x0,y0,hourVXY$x,hourVXY$y)  

}

## function to grab par's usr param for use in subsequence plots
usr2lims <- function(adj=.04){
  
  par.usr <- par('usr')
  xlims <- par.usr[c(1,2)]
  ylims <- par.usr[c(3,4)]

  adj <- adj - adj^2 *2 # this simplifies the math below
  
  xlims <- xlims + c(adj,-adj) *  diff(xlims)
  ylims <- ylims + c(adj,-adj) *  diff(ylims)

  return(list(x=xlims, y=ylims))
}



#annulus()  #donut or ring plot


vennMatrix <- function(l){
  ## much of the code in this function was inspired by parts of Yongmin Sun's doVennDiagram function
  if(is.null(names(l)))
    stop("The list 'l' must have named elements")
  l.all <- unique(do.call(c,lapply(l, as.character)))
  l.mat <- matrix(0, nrow = length(l.all), ncol = length(l))
  colnames(l.mat) <- names(l)
  rownames(l.mat) <- l.all

  for(i in 1:length(l.all)) 
    for(nm in names(l))
      l.mat[i,nm] <- l.all[i] %in% l[[nm]]
  return(l.mat)
}



textplot <- function(..., x=1, y=1){
  plot(x, y, pch='', bty='n',xaxt='n',yaxt='n', xlab='', ylab='')
  text(x, y, ...)
}




mvlabs <- function(df, n=nrow(df), x='x', y='y', l='lab', cols=colors()[grep("dark",colors())], ...){

  for(i in (1:n)+1){

    ## identify point to move
    idx <- identify(x=df[,x], y=df[,y], labels=df[,l],n=1)
    print(df[idx,])
    ## locate new location to move to
    locs <- locator(n=1)

    ## move the point  (refresh the plot?)
    df[idx, x] <- locs$x[1]
    df[idx, y] <- locs$y[1]
    #points(x=df[idx, x], y=df[idx, y], col=colors()[i])
    text(x=df[idx, x], y=df[idx, y], labels=as.character(df[idx,l]), col=cols[i], ...)

  }
  return(df)
}



labsegs <- function(x0, y0, x1, y1, buf=.3, ...){

  a <- x1 - x0
  b <- y1 - y0
  c0 <- sqrt(a^2 + b^2)
  theta <- atan(b/a)
  theta[a<0] <- theta[a<0] + pi
  
  c1 <- c0 - buf
  
  if(any(c1<0))
    stop('buffer size too large or annotations too close')
  
  a1 <- c1*cos(theta)
  b1 <- c1*sin(theta)

  x1 <- x0 + a1
  y1 <- y0 + b1

  segments(x0,y0,x1,y1,...)
}


heatmatrix <- function(x, values=TRUE, clp=c('bottom','top'), rlp=c('left','right'), xadj=.02, yadj=.3, ylab.cntr=FALSE, cex=1, cex.axis=1, ...){

  image(1L:ncol(x), 1L:nrow(x), t(x[nrow(x):1,]),  xaxt='n', yaxt='n', ... )

  if(!is.null(rownames(x))){
    clp2par <- nv(c(1,2),clp)
    clp2xadj <- nv(c(-1,1),clp) * xadj
    clp2adj <- nv(c(1,0),clp); if(ylab.cntr){ clp2adj <- nv(rep(.5,2),clp)}
    clp <- match.arg(clp)
    text(x=par("usr")[clp2par[clp]] +clp2xadj[clp], y=nrow(x):1, adj=clp2adj[clp], labels = rownames(x), xpd = TRUE, cex=cex.axis)
  }
  if(!is.null(colnames(x))){
    rlp2par <- nv(c(3,4),rlp)
    rlp2yadj <- nv(c(-1,1),rlp) * yadj
    rlp <- match.arg(rlp)
    text(x=1:ncol(x), y=par("usr")[rlp2par[rlp]] + rlp2yadj[rlp], adj=.5,
         labels = colnames(x), xpd = TRUE, cex=cex.axis)
  }
  if(values)
     text(col(x),row(x), round(x[nrow(x):1,],2), cex=cex)

}


## function modified from stackoverflow (via chan1142)
legend.position <- function(x,y,xlim=NULL,ylim=NULL, start=.05, end=.5, incr=.01) {
  if (dev.cur() > 1) {
    p <- par('usr')
    if (is.null(xlim)) xlim <- p[1:2];x1<-xlim[1];x2<-xlim[2]
    if (is.null(ylim)) ylim <- p[3:4];y1<-ylim[1];y2<-ylim[2]
  } else {
    if (is.null(xlim)) xlim <- range(x, na.rm = TRUE)
    if (is.null(ylim)) ylim <- range(y, na.rm = TRUE)
  }
  .sumup.points <- function(f) {
     tl <- sum((x <= (x1+(x2-x1)*f)) & (y >= (y2-(y2-y1)*f)))
     bl <- sum((x <= (x1+(x2-x1)*f)) & (y <= (y1+(y2-y1)*f)))
     tr <- sum((x >= (x2-(x2-x1)*f)) & (y >= (y2-(y2-y1)*f)))
     br <- sum((x >= (x2-(x2-x1)*f)) & (y <= (y1+(y2-y1)*f)))
    c(topleft=tl,topright=tr,bottomleft=bl,bottomright=br)
  }
  A <- rep(0,4)
  fractionations <- seq(start,0.5,by=incr)
  for (f in fractionations) {
     a <- .sumup.points(f)
     A <- rbind(A,a); 
    if (sum(a!=0)==4) break
  }
  corner.means <- apply(A, 2, function(x) weighted.mean(x,nrow(A):1));
  colnames(A)[which.min(corner.means)][1]
}


# split plot to investigate confounding between modeled variables (inspired by lattice's multi-panel lattice graph)
plot.confound.grid <- function(x,Y='y',X='x',confounder='z',breaks=3, mains='breaks', ...){
 oldpar <- par(no.readonly=TRUE);
 par(mfrow=c(1,breaks))
 on.exit(par(oldpar))

 if(length(breaks)==1)
  breaks   <- c(quantile(  x[,confounder], probs = seq(0, 1, by = 1/breaks),na.rm=TRUE))
 ecs   <- split(x,cut(  x[,confounder], breaks=breaks));

 ellipsis <- ellipsis.defaults(x=list(...), nl=list(ylab=Y,xlab=X))

 if(mains=='breaks')
  mains <- paste(confounder, as.character(names(ecs)))
 if(length(mains)!=length(ecs)){
  warning('mains must be a vector of plot titles equal to the breaks')
  mains <- rep(mains,length(ecs))
 }
 for(i in 1:length(ecs)){ 
  #with(ecs[[i]], plot(get(X),get(Y),  opt.args)))  
  do.call(plot, c(list(x=ecs[[i]][,X],y=ecs[[i]][,Y], main=mains[i]),  ellipsis)) 
  with(ecs[[i]],abline(lm(get(Y)~get(X))))
 }
}

ellipsis.defaults <- function(x, nl){
  for(i in seq(along.with=nl)){
    if(is.na(match(names(nl)[i], table=names(x)))){ 
        x <-  c(unlist(x), nl[i])
   }
 }
 return(x)
}

