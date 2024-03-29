
.spie <- function(p1, p2, init.angle=pi, plot=TRUE){
  
  if(length(p1)!=length(p2))
    stop("'p1' and 'p2' have different lengths")

  #if(sum(p1)!=sum(p2))
  #  stop("'p1' and 'p2' doesn't sum the same")

  angles <- cumsum(c(init.angle, 2 * pi * p1 / sum(p1)))

  radii  <- sqrt( (p2 / sum(p2)) /
                  (p1  / sum(p1)) )

  namesSlices <- if(is.null(names(p1)))
                   1:length(p1)
                 else
                   names(p1)

  str <- structure(list(angles=angles,
                 radii=radii,
                 p1=p1,
                 p2=p2,
                 namesSlices = namesSlices),
            class="spie")
  #if(plot)
  #  plotspie(str, ...)
  #invisible(str)
  return(str)
}


spie <- function(p1, p2, init.angle=pi, multi, col = rainbow(length(x$radii)), bg=col, lwd=2, pie.labs=TRUE, grid=TRUE, grid.labs=TRUE, scale=TRUE, p1.circle=TRUE){
  
  x <- .spie(p1, p2, init.angle=pi)
  
  requireNamespace("grid")
  maxRadii <- max(x$radii)

  grid::grid.newpage()

  grid::pushViewport(grid::viewport(layout=grid::grid.layout(1,1,respect=TRUE)))
  grid::pushViewport(grid::dataViewport(maxRadii*c(-1.1,1.1),
                            maxRadii*c(-1.1,1.1),
                            layout.pos.col=1,
                            layout.pos.row=1))
  if(!missing(multi) & p1.circle)
      grid::grid.circle(x=0, y=0, r=sqrt(multi), gp=grid::gpar(col="gray"), default.units="native")

  for(i in 1:length(x$radii)){
  
    #this check is ugly i know, may move/refactor it 
    if(length(lwd) > 1){
     if(length(lwd) != length(x$radii)){
        stop('lwd must have a length = to the number of partitions')
      }else{
        this.lwd <- lwd[i]
      }
    }else{
      this.lwd <- lwd
    }
    
    theta <- seq(x$angles[i], x$angles[i+1], length=100)

    if(p1.circle){
    ## background p1 circle
    grid::grid.polygon(x   = c(0,  cos(theta) ,0),
                 y   = c(0,  sin(theta) ,0) ,
                 gp  = grid::gpar(fill=bg[i]),
                 default.units="native")
    }                 
    ## superimposed p2 circle
    grid::grid.polygon(x   = c(0, x$radii[i] * cos(theta) ,0),
                 y   = c(0, x$radii[i] * sin(theta) ,0) ,
                 gp  = grid::gpar(fill=col[i], lwd=this.lwd),
                 default.units="native")


    angleAnn <- mean(x$angles[i+0:1])
    maxx <- max(1, x$radii[i])+maxRadii/10

    if(pie.labs){

       grid::grid.rect( x = cos(angleAnn)*maxx,
                 y = sin(angleAnn)*maxx,
                 width = 1.5*grid::stringWidth(x$namesSlices[i]),
                 height = 1.5*grid::stringHeight(x$namesSlices[i]),
                 default.units="native",
                 gp = grid::gpar(col=col[i], fill="white", lwd=2))
 

       grid::grid.text(x$namesSlices[i],
                x=cos(angleAnn)*maxx,
                y=sin(angleAnn)*maxx,
                default.units="native")
    }
  }

  if(!missing(multi)){

   if(grid){
      grid::grid.lines(x=grid::unit(0,"native"),
                 y=grid::unit(c(0, max(sqrt(multi))), "native"), gp=grid::gpar(col="gray"))

      if(grid.labs){
        for(i in multi){
          st <- paste("x", i)
          sw <- grid::stringWidth(st)
          sh <- grid::stringHeight(st)
  
          grid::grid.rect( x = grid::unit(0, "native"),
                   y = grid::unit(sqrt(i),"native"),
                   width = 1.5*sw,
                   height = 1.5*sh , gp=grid::gpar(fill="white", col="gray"))
          grid::grid.text( st , 0, sqrt(i), default.units="native")

        }
      }
    }

  }

  grid::upViewport(2)

}
