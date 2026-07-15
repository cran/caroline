\name{pct}
\alias{pct}
\title{Add Percentage Columns to a Dataframe}
\description{
This function will add extra columns to an existing dataframe.  The second argument 'clmn' should specify which column(s) of the dataframe the percentage should be calculated by dividing each column's row-element by it's sum. If x is not a dataframe, it will attempt to do a simple percentage operation on whatever input was passed to x.}
\usage{
pct(x, clmns=NA, digits=2, suffix='\%')
}
\arguments{
\item{x}{A dataframe with numeric columns, or just a single numeric vector.}
\item{clmns}{the names of the columns for which the percentage column should be calculated from.}
\item{digits}{number of decimal places for rounding.}
\item{suffix}{a character string specifying that the fraction should be converted to a true percentage, as a character string, with a percentage-worthy suffix.}
}
\value{
The original dataframe plus extra percentage columns corresponding to original columns in the dataframe.
}
\examples{

df <- data.frame(a=c(1,2,3), b=c('x','y','z'), c=c(5,3,2))
pct(df, c('a','c'))

pct(1:10)

}
\keyword{manip}

