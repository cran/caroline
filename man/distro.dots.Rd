\name{distro.dots}
\alias{distro.dots}
\title{Distribution plot of points}
\description{
Plot the raw distribution of points, like a series of horizontal box plots.
}
\usage{
distro.dots(x, jit.f=1, add=FALSE, pd=0,  vv=names(x), vvlabs=NULL,
                  xlim=range(unlist(x)), ...)
}
\arguments{
\item{x}{a list of vectors of values to be plotted}
\item{jit.f}{factor for random jittering (see 'jitter()'}
\item{add}{should we add to the existing plot?}
\item{pd}{'position dodge' moves all y axis plotting positions up or down by this provided value (useful for adding multiple distributions for the same variable)}
\item{vv}{the variable vector for ordering the y-axis labels}
\item{vvlabs}{the variable vector labels for labeling the plot (defaults to vv)}
\item{xlim}{x axis plot limits}
\item{...}{other parameters passed on to plot}
}
\value{
a 'distro dot plot' of variable distributions
}
\examples{

n <- rnorm(130, 10, 3)
p <- rpois(110, 4)
u <- runif(300, 0, 20)
l <- rlnorm(130, log(2))
g <- rgamma(140, 3)

X <- list(a=u, random=n, array=p,of=l, variable=u, spreads=g)
distro.dots(x=X, jit.f=3)

}
