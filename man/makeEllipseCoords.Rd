\name{makeEllipseCoords}
\alias{makeEllipseCoords}
\title{Make Ellipse Coordinates}
\description{
Create x & y coordinates for an ellipse from parameters.
\code{save}.
}
\usage{
makeEllipseCoords(x0 = 0, y0 = 0, b = 1, a = 1, alpha = 0, pct.range = c(0,1), len = 50)
}
\arguments{
\item{x0}{x coordinate of center of ellipse.}
\item{y0}{y coordinate of center of ellipse.}
\item{b}{y axis stretch factor.}
\item{a}{x axis stretch factor.}
\item{alpha}{rotation factor.}
\item{pct.range}{percentage of the way around the ellipse.}
\item{len}{number of points used to draw ellipse.}
}
\value{
a 2 column (x and y) dataframe with coordinates for drawing an ellipse.
}
\examples{
makeEllipseCoords(x0 = 0, y0 = 0, b = 1, a = 2, alpha = 0)
}
\keyword{graphs}

