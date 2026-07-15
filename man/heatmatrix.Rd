\name{heatmatrix}
\alias{heatmatrix}
\title{Simple Heatmap Plot}
\description{
This is a very simplified heat-map function--a convenient wrapper around the 'image' function.  
}
\usage{
heatmatrix(x, values=TRUE, clp=c('bottom','top'), rlp=c('left','right'), 
  xadj=.05, yadj=1, ylab.cntr=FALSE, cex=1, axis.cex=1, text.col=1, xlab='',ylab='',...)
}
\arguments{
\item{x}{A matrix}
\item{values}{boolean: should the values be plotted over each cell?}
\item{clp}{column label position: either 'bottom' or 'top'.}
\item{rlp}{row label position: either 'right' or 'left'.}
\item{xadj}{x-adjust of the row labels.}
\item{yadj}{y-adjust of the column labels.}
\item{ylab.cntr}{boolean for justification of row labels.}
\item{cex,}{character expansion factor for values in cells if values == TRUE.}
\item{axis.cex}{character expansion factor for axis tick mark labels.}
\item{text.col}{color of the text printed over the colored rectangle of all matrix cells.}
\item{xlab}{x axis label.}
\item{ylab}{y axis label.} 
\item{...}{other parameters passed on to image().}
}
\seealso{
\link{image}, \link[stats]{heatmap}, \link[gplots]{heatmap.2}
}
\value{
A heat-map plot.
}
\examples{

data(mtcars)
x  <- as.matrix(mtcars)

heatmatrix(x)

}
