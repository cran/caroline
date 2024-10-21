

# plot with a linear model trendline (and its p-value reported as text overlay on the plot)
plot.xy.ab.p <- function(x, x.var, y.var, fit.line=TRUE, p.value=TRUE, slope=TRUE, p.col=c('red','gray','green'), plot.labels=TRUE, verbose=TRUE, xlab=x.var, ylab=y.var, ...){
  if(verbose) print(paste('x=',x.var,'; y=',y.var))
  pch<-par()$pch
  if(plot.labels!=FALSE & !is.na(plot.labels) & !is.null(plot.labels)){
    if(length(plot.labels)>1){
      if(length(plot.labels) == nrow(x)){
       label.vector <- plot.labels
      }else{
       stop("plot label vector length does not match rows in x")
      }
    }else{
     if(is.character(plot.labels)){
       if(!plot.labels %in% names(x)) stop('cannot find plot.labels in (col)names of x')
       label.vector <- with(x,get(plot.labels))
     }else{
       label.vector <- rownames(x); message("plot.labels=TRUE so using rownames(x)")
     }
    } 
    pch <- ''      
  }

  with(x, plot(x=get(x.var), y=get(y.var), xlab=xlab, ylab=ylab, pch=pch, ...))
  if(!is.null(label.vector) & pch=='') 
  with(x, text(get(x.var), get(y.var), labels=label.vector, ...))

  model.formula <- as.formula(paste(y.var,'~',x.var))
  linear.model <- lm(model.formula,data=x)
  if(!any(is.na(linear.model$coefficients))){
   p <- round(summary(linear.model)$coefficients[2,4],3)
   B <- round(summary(linear.model)$coefficients[2,1],5)
   B.sign <- ifelse(test=B>0, yes="+", no="-")
   if(p.value){ print(paste(B.sign,"p-value=",p), digits=3)
                with(x,text(mean(get(x.var),na.rm=TRUE),
                  mean(get(y.var),na.rm=TRUE),labels=paste('p=',p),col=p.col)) } 
   if(slope){ print(paste("slope=",B), digits=3)
                with(x,text(mean(get(x.var),na.rm=TRUE),
                  mean(get(y.var),na.rm=TRUE),labels=paste('slope=',B),col=p.col)) } 
   #if(length(p.col)>1){p.col <- colorRampPalette(p.col)(5);pcolidx=(round(atan(B)*2)+3)}else{pcolidx=1}
   if(length(p.col)==3){p.col <- p.col[as.integer(B>0)*2+1]}
   if(fit.line) with(x, abline(linear.model, col=p.col, lwd=-log(summary(linear.model)$coefficients[2,4])+.3))
  }
}

