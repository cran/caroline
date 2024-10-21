\name{ellipsis.defaults}
\alias{ellipsis.defaults}
\title{Modify defaults of possible optional ellipsis parameter values passed to a function}
\description{
Takes the ellipsis function parameter (of optional named arguments) and compares with a internal (named list) of defaults for any non-conflicting user specified inputs. 
}
\usage{
ellipsis.defaults(x, nl)
}
\arguments{
\item{x}{the existing ellipsis passed as "list(...)"}
\item{nl}{a named list of default parameter values}
}
\value{
a modified ellipsis variable (a named list of optional parameter values)
}
\examples{


domy.xyplot <- function(data, x.name, y.name, ...){

 ellipsis <- ellipsis.defaults(x=list(...), nl=list(ylab=y.name,xlab=x.name))
 do.call( plot,  c(list(x=data[,x.name],y=data[,y.name]),  ellipsis))

}

domy.xyplot(data=mtcars, y.name='mpg', x.name='wt', xlab='weight')

}
