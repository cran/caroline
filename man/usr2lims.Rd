\name{usr2lims}
\alias{usr2lims}
\author{David Schruth <dschruth@u.washington.edu>}
\title{Grab and adjust the current plot dimensions}
\description{
This is a simple function which grabs the current plot dimentions and adjusts them by shrinking them by 4% in each direction.
}
\usage{
usr2lims(adj=.04)
}
\arguments{
\item{adj}{The automatic adjustment factor 'plot' adds to buffer the specified plot dimentions.}
}
\value{
A list of the new xlim and ylim vectors.
}
\seealso{
par
}
\examples{
}
\keyword{}