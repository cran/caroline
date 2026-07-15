\name{nv2df}
\alias{nv2df}
\author{David Schruth}
\title{Create a data frame from a named vector}
\description{
This function simply takes the values of the input vector and assigns it to the first column of the dataframe. The second column assigned from the names of this input vectors).
}
\usage{
nv2df(x, clmn.names=c('x','name'), ...)
}
\arguments{
\item{x}{a named vector.}
\item{clmn.names}{a vector of names of the new columns formed by 1) the values and 2) the names of the named vector.}
\item{...}{other parameters passed on to data.frame().}
}
\value{
a two column dataframe with values of x in the first column and the names of x in the second
}
\seealso{
\link{vector},\link{name}
}
\examples{

x <- 1:3
nv2df(nv(x,letters[x]), clmn.names=c('x','y'))

}
