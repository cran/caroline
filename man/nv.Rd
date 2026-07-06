\name{nv}
\alias{nv}
\author{David Schruth}
\title{Create a named vector from a dataframe, table or vector}
\description{
The '$' or "[," operators for dataframes and tables do not carry along with them the row names.  This function provides a solution for this problem.  Additionally this function will accept a vector of values and a corresponding vector of value names--an ideal, in-line way for setting named-vectors as default parameters in new functions. If x is not specified, x will be populated with an integer vector of the same length as name. Now there is also a way to use nv to retrive named vectors from two-colunn lookup tables using the new paramter key.clmn, which defaults to 'id')
}
\usage{
nv(x= seq(along = name), name = letters[length(x)], key.clmn = "id")
}
\arguments{
\item{x}{The source dataframe, table, vector, or factor}
\item{name}{a vector of length two specifying the data and name colunmns of xas a dataframe OR an [x-length] vector of names of vector x (if a vector) OR a vector [of length=2] specifying the columns (values & names) of x passed in as a dataframe}
\item{key.clmn}{a string specifying which column name in x (only when x is a two-column [lookup] data.frame) are values, where the other clmn are used as names for these values.}
}
\value{
a named vector or factor
}
\seealso{
\link{vector},\link{name}
}
\examples{

## example 1: pulling a row.named vector out of a dataframe
df <- data.frame(a=c(1,2,34,4,5,3,23), b=c('z','x','y','z','x','n','p'))
rownames(df) <- letters[1:nrow(df)]
nv(df,'a')
nv(df,'b')

## example 2: a naming vectors from scratch
nv(c(1,2,3), c('a','b','c'))
nv(df$a, df$b)

## using two column names to create: a vector of values and names

## auto-numbering for data named by the same length of 'name'
nv(name=c('a','b','c'))

}
