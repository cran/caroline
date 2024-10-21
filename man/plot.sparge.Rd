\name{plot.sparge}
\alias{plot.sparge}
\alias{sparge}
\alias{sparge.plot}
\title{Visually compare all points from different univariate distributions}
\description{
Visually compare continuuous univariate distributions using jittered and [progressive levels of] transparent points. This type of diagram plots positions of raw numerical data of comparable univariate distributions with a boxplot overlay indicating quartiles surrounding the central tendency of the underlying points. The distributions are vertically stacked (between) and  jittered (within) as well as translucent in order to reduce overlapping points on larger-|N| datasets. 
}
\usage{
\method{plot}{sparge}(x, f=NULL, out.range=range(unlist(x)), cat.names=names(x), 
           cpd=0, cpw=.4, jit.f=1, horiz=TRUE, add=FALSE, lgnd='auto', zl=FALSE,  
                                        col=1, box.brdrs='gray',alpha=.3, ...)
 }
\arguments{
\item{x}{a list of numeric vectors OR a dataframe with both numeric and factor columns}
\item{f}{either a factor [that is same length as a numeric 'x'] OR a model formula}
\item{out.range}{range of all possible outcome variable values (recursive loop prespecification)}
\item{cat.names}{level names of the primary categorical variable partitioning the distributions}
\item{cpd}{position dodge: shifts all categorical plotting positions this factor}
\item{cpw}{position width: width of the swath of jittered categorical positions}
\item{jit.f}{factor for random jittering (see 'jitter()'}
\item{horiz}{should rotate existing plot horizontally? (be sure to double check x & y labels match)}
\item{add}{should we add to the existing plot?}
\item{lgnd}{added automatically by default but can be suppressed by setting to NULL or FALSE}
\item{zl}{should we add a horizontal [zero] line at x=0?}
\item{col}{(vector of) [base] colors of the points of the distribution(s)}
\item{box.brdrs}{the color of the borders of the box plots surrounding all distributions}
\item{alpha}{transparency level for [overlapping] points}
\item{...}{other parameters passed on to plot}
}
\details{
The function can currently take three different forms of input. First, x can be a list of numeric vectors with no need for f. Second, x can be a single vector that is to be split by factor f, of the same length. Third, x can be a dataframe and f specifies a model formula in the form of "outcome ~ control" (simple plot) or "out ~ predictor | control" (two series plot with legend).
}
\value{
a 'sparge' [sprinkler/smear] plot of point distributions
}
\seealso{
See also 'boxplot' and 'stripchart' in package 'graphics' as well as 'sina', 'violin', 'bean', 'ridgelines', and 'raincloud' plots.
}
\examples{

x <- lapply(sample(1:5), function(avg) (rnorm(500,avg)))
names(x) <- letters[1:length(x)]
plot.sparge(x, col=rep('blue',length(x)), main='sparge plots:\nfor distributional comparison')

## four random distributrions (from the 'boxplot' examples)
distros <- list(Uni05 = (1:100)/21, Norm = rnorm(100), `5T` = rt(100, df = 5), 
                                               Gam2 = rgamma(100, shape = 2))
plot.sparge(distros, ylab='distribution',xlab='')


# three more random distributions (from the 'sinaplot' examples)
bimodal <- c(rnorm(300, -2, 0.6), rnorm(300, 2, 0.6))
uniform <- runif(500, -4, 4)
normal <- rnorm(800,0,3)
distributions <- list(uniform = uniform, bimodal = bimodal, normal = normal)
plot.sparge(distributions, ylab='distribution',xlab='')


## using 'f' [as a factor] argument as grouping factor on just one treatment

# Orchard spray by treatment (compare with 'strip chart' plot)
OS <- with(OrchardSprays, split(decrease, treatment))
plot.sparge(OS, log = "x", main = "OrchardSprays", xlab='decrease',ylab='treatment')

# Tooth Growth
plot.sparge(x=ToothGrowth$len, f=ToothGrowth$sup, xlab='lenght', ylab='supplement')


# multi-predictor using model-based parsing of 'f' [as a formula] and 'x' as a dataset

# Tooth Growth 
plot.sparge(x=ToothGrowth, f="len ~ dose | supp", xlab='dose',ylab='tooth length', horiz=FALSE)
# or model-based with out the supplement sub-splitting
plot.sparge(x=ToothGrowth, f="len ~ dose",  xlab='dose',ylab='tooth length', horiz=FALSE)

# from the CO2 dataset
plot.sparge(CO2, 'uptake ~ Type | Treatment', horiz=FALSE,
            xlab='Type',ylab='Uptake', main='CO2')

# Joyner-Boore earthquake data (heavily rounded)
attenu$magnitude <- as.factor(round(attenu$mag))
attenu$distance <- as.factor(round(log10(attenu$dist)))
plot.sparge(x=attenu, f="accel ~ distance | magnitude", horiz=FALSE, 
  xlab='log10(distance)',ylab='acceleration', main='earthquake attenuation')

# Motor Trend cars data (rounded)
mtcars$cylinders <- as.factor(mtcars$cyl)
plot.sparge(x=mtcars, f="qsec ~ gear | cylinders", horiz=FALSE, 
         xlab='number of gears', ylab='seconds', main='Motor Trend Cars')

# fertility dataset 
infert$education <- as.factor(infert$education)
infert$ages <- jitter(infert$age, amount=1/2)
plot.sparge(x=infert, f="ages ~ spontaneous | education ", horiz=FALSE, 
       ylab='[jittered] ages, yrs', xlab='spontaneous' , main='fertility')


}
