\name{plot.xy.ab.p}
\alias{plot.xy.ab.p}
\alias{xy.ab.p}
\title{An x/y scatterplot with a linear regression line and p-value}
\description{
This function performs a simple scatter plot but also superimposses a linear regression trend (abline) and optionally also the p-value of this line
}
\usage{
\method{plot}{xy.ab.p}(x, x.var, y.var, 
fit.line=TRUE, p.value=TRUE, slope=TRUE, p.col=c('red','gray','green'), 
               plot.labels=TRUE, verbose=TRUE, xlab=x.var, ylab=y.var, ...)
}
\arguments{
\item{x}{a data frame}
\item{x.var}{the name of the x variable in df}
\item{y.var}{the name of the y variable in df}
\item{fit.line}{should a fit (ab) line be drawn?}
\item{p.value}{should the p-value be printed on the plot?}
\item{slope}{should the slope be printed on the plot?}
\item{p.col}{should the plot be labeled?}
\item{plot.labels}{should all of thie model fit information be printed out?}
\item{verbose}{should all other information be printed out too?}
\item{xlab}{label for x-axis}
\item{ylab}{label for y-axis}
\item{...}{other parameters passed to 'plot'}
}
\value{
An x/y scatterplot with regression line
}
\examples{


par(mfrow=c(6,5), mar=c(2,2,0,0), mgp=c(-1,.5,0))

eg.df <- expand.grid(names(swiss), names(swiss))
for(i in 1:nrow(eg.df)){
  print(i)
  xv <- as.character(eg.df[i,1]); print(xv)
  yv <- as.character(eg.df[i,2]); print(yv)
  if(yv != xv)
    plot.xy.ab.p(x=swiss, x.var=xv, y.var=yv, p.value=FALSE, slope=FALSE) 
}

}
