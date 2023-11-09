\name{plot.sparge}
\alias{plot.sparge}
\alias{sparge}
\alias{sparge.plot}
\title{Visually compare all points from different univariate distributions}
\description{
Visually compare continuuous univariate distributions using jittered and transparent points.
}
\usage{
\method{plot}{sparge}(x, jit.f=1, zl=TRUE, xlim=range(unlist(unlist(x))), 
                  add=FALSE, pd=0, box.brdrs='gray', col=1, alpha=.3, ...)
}
\arguments{
\item{x}{a list of vectors each with values drawn from the same distribution}
\item{jit.f}{factor for random jittering (see 'jitter()'}
\item{zl}{should we add a horizontal [zero] line at x=0?}
\item{add}{should we add to the existing plot?}
\item{pd}{'position dodge' moves all y axis plotting positions up or down by this factor}
\item{xlim}{x axis plot limits}
\item{alpha}{transparency level for [overlapping] points}
\item{box.brdrs}{the color of the borders of the box plots surrounding all distributions}
\item{col}{(vector of) [base] colors of the points of the distribution(s)}
\item{...}{other parameters passed on to plot}
}
\value{
a 'sparge' [sprinkle/smear] plot of point distributions
}
\seealso{
See also 'boxplot' and 'stripchart' in package 'graphics' as well as 'violin', 'bean', 'ridgelines', and 'raincloud' plots.
}
\examples{

N=300
x=lapply(sample(1:5), function(avg) (rnorm(N,avg)))
plot.sparge(x, col=rep('blue',length(x)), main='sparge plots:\nfor distributional comparison')


}
