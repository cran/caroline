plot.sparge <- function(x, f=NULL, out.range=range(unlist(x)), cat.names=names(x), cpd=0, cpw=.4, jit.f=1, horiz=TRUE, add=FALSE, lgnd='auto', zl=FALSE,  col=1, box.brdrs='gray',alpha=.3, ...){

   ## INPUT AS FORMULA & DATA ###########################
   if( is.character(f) & is.data.frame(x) ){
      f <- as.formula(f)
      if( length(f) != 3)
        {stop("formula is malformed")}
      formula.parts <- as.character(f)
      formula.predictors <- strsplit(formula.parts[3], '[ |]+')[[1]]
      if( length(formula.predictors)==1 ){
       f <- as.factor(x[,formula.parts[3]])
       x <-           x[,formula.parts[2]]; 
       # qualifies for "input as vectors" #
      }else{
       if(length(formula.predictors) != 2)
        {stop("something is wrong with your formula: too many terms?")}
       out <- formula.parts[2]; # outcome variable
       fp1 <- formula.predictors[1] ; # main predictor
       fp2 <- formula.predictors[2] ; # control factor
       # TODO: add check so that xlab & ylab match terms (at least somewhat)
       if(!is.factor(x[,fp1]))
        {message("term before '|' should be a factor, coercing")}
        x[,fp1] <- as.factor(x[,fp1])
        fp1.levs <- levels(x[,fp1])
       if(!is.factor(x[,fp2]))
        {stop("term after '|' must be a factor")}
       fp2.levs <- levels(x[,fp2])
       fp2.l.ct <- length(fp2.levs)
       if(fp2.l.ct  > 6)
        {stop("too many levels in factor fp2")}
       if(length(col)==fp2.l.ct)
         lev.cols <- col
       else
         lev.cols <- nv(rainbow(fp2.l.ct), fp2.levs)
       pds <- nv(seq(-.1*fp2.l.ct/2, .1*fp2.l.ct/2, along.with=fp2.levs), fp2.levs)
       news <- nv(c(F,rep(T, fp2.l.ct -1)), fp2.levs)
       #axts <- nv(c('s',rep('n', fp2.l.ct -1)), fp2.levs)
       for(fp2lev in fp2.levs){
          # qualifies for "input as list of vectors" #
          x.sub <- subset(x, x[,fp2]==fp2lev)
          x.list <- split(x.sub[ , eval(out)], x.sub[, eval(fp1)]) 
          # RECURISVE CALL TO PLOT.SPARGE
          plot.sparge(x.list, out.range=range(x[,out]),  cat.names=fp1.levs,
                      col = lev.cols[fp2lev], cpd=pds[fp2lev], add=news[fp2lev], cpw=cpw/fp2.l.ct, #xaxt=axts[fp2lev], axt=axts[fp2lev],
                                                             horiz=horiz, zl=zl, jit.f=jit.f, box.brdrs=box.brdrs, alpha=alpha, ...)
       }
       if(lgnd=='auto'){
        if( horiz){low.dense.lgnd.pos <- legend.position( x[,out], as.numeric(x[,fp1]) )}
        if(!horiz){low.dense.lgnd.pos <- legend.position( as.numeric(x[,fp1]), x[,out] )} 
        legend(x=low.dense.lgnd.pos, legend=fp2.levs, pt.bg=lev.cols, pch=21, col='gray',inset=.1, title=fp2)
       }
       x <- NULL
       f <- NULL
      }
    } # (end formula input option [potentially recursive call])

   if( !(is.null(x) & is.null(f)) ){   # to handle exit from [potentally recursive] FORMULA option above
      
   ##  INPUT AS VECTORS (outcome + factor ONLY) #############
   if(is.factor(f) & !is.list(x)){
    if(length(f) != length(x))
      {stop("factor f must be same length as vector x")}
    x <- split(x,f)
    names(x) <- cat.names <- levels(f)
    # qualifies for "input as list [of vectors]" #
   } #(end vector input option conversion)

   ## INPUT AS LIST ##########################################
   if(!(is.list(x) & all(sapply(x,is.numeric))))
     {stop("x should be a list split into numeric vectors")}
      
   if(length(col)==1){ col <- rep(col,length(x))}
   # convert to transparent colors
   cols <- sapply(col, function(cl){ RGB <- col2rgb(cl)/255; rgb(RGB[1,],RGB[2,],RGB[3,], alpha=alpha)})

   q <- length(x)

   if(is.null(names(x)))
     {stop('x must be a named list')}
   #if(is.null(cat.names)){
   #  message('names of the levels of the primary categorical variable is not optional, using default on x (as list)')
   #   if(is.null(names(x))){cat.names <-1:length(x)}else{cat.names <-names(x)}
   #}

   pred.levels <- as.numeric(as.factor(cat.names))
   pred.pos.range <- c(min(pred.levels)-abs(cpw), max(pred.levels)+abs(cpw))
   pred.positions <- lapply(1:q, function(pp) jitter(rep(pp,length(x[[pp]])), factor=jit.f, amount=cpw/2) + cpd)

   xxt <- yxt <- 's'  
   
   unlist(pred.positions)
   if(horiz==TRUE){
     xs <- unlist(x)
     ys <- unlist(pred.positions)
     xlim <- out.range
     ylim <- pred.pos.range
     cat.axis.label.side <- 2
     yxt='n'
   }else{
     xs <- unlist(pred.positions) # switch which is 'xs' and which is 'ys'
     ys <- unlist(x)
     xlim <- pred.pos.range
     ylim <- out.range
     cat.axis.label.side <- 1
     xxt <- 'n'
   } ## TODO: send x & y labels from parsed model terms
   par(new=add)
   plot(xs, ys, ylim=ylim, xlim=xlim, xaxt=xxt, yaxt=yxt, pch=21, bg=cols, ...) 
   axis(side=cat.axis.label.side, at=pred.levels, labels=cat.names)

print(pgeom(length(xs)/3, prob=.03) -.1)
   gray.scale <- gray(pgeom(length(xs), prob=.02) -.1)
   if(!is.null(box.brdrs)){
    if(box.brdrs=='gray'){ bc = gray.scale }else{ bc=box.brdrs}
     boxplot(x, at=1:q+cpd, range=0, yaxt='n', xaxt='n', horizontal=horiz, col='transparent', varwidth=T, las=1, add=TRUE, border=bc, boxwex=cpw)
   }
  if(zl){abline(v=0, lty=2)}  
 } # end list-only input option 
} # end function





