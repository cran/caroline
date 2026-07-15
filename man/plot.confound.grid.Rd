\name{plot.confound.grid}
\alias{plot.confound.grid}
\alias{confound.grid}
\title{Plot a grid of x y plots split by a confounder z}
\description{
Plot a grid of x y plots showing how a third confounding variable 'z' changes the slope.
}
\usage{
\method{plot}{confound.grid}(x, f, breaks=4, mains=NA,...)
}
\arguments{
\item{x}{a data frame}
\item{f}{a formula of the form "Y ~ X | confounder" with names that correspond to names of the dataframe columns.}
\item{breaks}{number or vector of breaks to split the plots horizontally (across x), automatically set for factors.}
\item{mains}{a vector plot title strings;
             defaults to the continuous->categorical break levels of 'confounder'}
\item{...}{other arguments passed to 'plot'}
}
\value{
A confounding diagnostic grid of plots.
}
\references{
 Simpson, E.H. (1951). "The Interpretation of Interaction in Contingency Tables".
 Journal of the Royal Statiatical Soceity, Series B.  13(2) p. 238-241.
}
\examples{

# Swiss Fertility dataset confounding example
plot.confound.grid(x=swiss, f="Infant.Mortality ~ Fertility | Education", breaks=4)

# Intertility dataset confounding example
infert$parity <- as.factor(infert$parity)
plot.confound.grid(x=infert, f="spontaneous ~ age |  parity")

}


