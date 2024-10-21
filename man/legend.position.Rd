\name{legend.position}
\alias{legend.position}
\title{automatically find an optimal position a plot legend based on point counts in each plot corner}
\description{
This function assesses four corners of an x vs y plot to find the one with the fewest points.  
It does so by iterating counting from outward to the middle, performing a sparseness-weighted average across corners.
}
\usage{
legend.position(x,y,xlim=NULL,ylim=NULL,start=.05, end=.5, incr=.01)
}
\arguments{
\item{x}{x values of the plot}
\item{y}{y values of the plot}
\item{xlim}{range of the x values}
\item{ylim}{range of the y values}
\item{start}{starting percentage for incremental counting from corner}
\item{end}{ending percentage for incremental counting (middle=0.5)}
\item{incr}{incremental step between each point counting iteration}
}
\value{
one of four values ('bottom'/'top' + 'right'/'left') corresponding to the optimal [most white space] position of the legend.
}
\seealso{
Hmisc's largest.empty function
}
\examples{

par(mfrow=c(3,3))

for(i in 1:9){
x <- rnorm(50)
y <- rlnorm(50)
clrs <- c('red','blue')
plot(x,y, col=clrs)
legend(x=legend.position(x,y), legend = c('x','y'), pch=1, col=clrs, inset=.1) 
}
}
