\name{plot.sparge}
\alias{sparge}
\alias{plot.sparge}
\alias{lines.sparge}
\alias{sparge.plot}
\alias{sparge.lines}
\title{Visually compare all points from different univariate distributions}
\description{
Visually compare continuuous univariate distributions using jittered and [progressive levels of] transparent points. This type of diagram plots positions of raw numerical data of comparable univariate distributions with a boxplot overlay indicating quartiles surrounding the central tendency of the underlying points. The distributions are vertically stacked (between) and  jittered (within) as well as translucent in order to reduce overlapping points on larger-N datasets. 

lines.sparge() is an accessory function that, like it's big brother (plot.sparge()) primarily relys upon data (x), a formula (f), and a point dodge factor (cpd), as well as an indication of which direction the plot is facing: sideways (horiz=T) or upwards (horiz=F). There are many params of plot.sparge() that lines.sparge() does not use (out.range, cpw, jit.f, add, lgnd, zl, boxol, alpha, wiskers) but only ones that are unique to lines.sparge(): cat.order, which determines if lines should connect accross groups (=1:2) or within groups (2:1), as well as cols and pds (similar to cpd, but vectorized on either side of 0 for two term models).
}
\usage{
\method{plot}{sparge}(x, f=NULL, cat.names=if(is.factor(f)){levels(f)}else{NULL}, 
          out.range=range(unlist(x)), cpd=0, cpw=.4, jit.f=1, horiz=TRUE, add=FALSE,  
          lgnd='auto', zl=FALSE, pt.cols=1, boxol='gray',alpha=.2, wiskers=1.5, ...)

\method{lines}{sparge}(x, f=NULL, pds, cat.order=1:2, 
         horiz=TRUE, rb=nv(rainbow(length(pds)),names(pds)),  pt.pch="+", pt.cex=1.5, ...)
}
\arguments{
\item{x}{a list of numeric vectors OR a dataframe with both numeric and factor columns}
\item{f}{EITHER a model formula OR a factor [that is same length as a single numeric vector defined by 'x'}
\item{cat.names}{ordered vector of level names of the primary categorical variable partitioning the distributions}
\item{cat.order}{a integer vector (length=2) passed only to lines.sparge() corresponding to a two-predictor model passed to f [as a formula]}
\item{out.range}{range of all possible outcome variable values (recursive loop prespecification)}
\item{cpd}{categorical position dodge: shifts all categorical plotting positions by this factor}
\item{pds}{position dodges returned from plot.sparge() and depended on by lines.sparge() to determine positions off of categorical/integers on the predictor axis (default=0)}
\item{cpw}{categorical position width: width of the (group of) swath(s) of jittered categorical positions (max=.5)}
\item{jit.f}{factor for random jittering (see 'jitter()'}
\item{horiz}{should rotate existing plot horizontally? (be sure to double check x & y labels match)}
\item{add}{should we add to the existing plot?}
\item{lgnd}{added automatically by default but can be suppressed by setting to NULL or FALSE}
\item{zl}{should we add a horizontal [zero] line at x=0?}
\item{pt.cols}{(vector of) [base] colors of the points of the distribution(s)}
\item{rb}{(cpd-length vector of colors, currently uses names (ignored if cat.order=2:1 and col is set}
\item{boxol}{the color of the outlines of the box plots surrounding all distributions (unless NULL, these will auto grayscale)}
\item{alpha}{transparency level for [overlapping] points}
\item{wiskers}{whiskers multiplier == X interquartile range. Same default as boxplot(range=1.5).}
\item{pt.pch}{point character for plot points in lines.sparge()}
\item{pt.cex}{character expansion multiplier for plot points in lines.sparge()}
\item{...}{other parameters passed on to [box]plot}
}
\details{
The function can currently take three different forms of input. First, x can be a list of numeric vectors with no need for f. Second, x can be a single vector that is to be split by factor f, of the same length. Third, x can be a dataframe and f specifies a model formula in the form of "outcome ~ predictor" (simple plot) or "out ~ predictor | control" (nested series plot with legend). The only extra points that are added to the plot are via the output of the boxplot() overlay: appearing as outliers (outside of the 99% normal probability range, as demarcated by the default wisker length) appearing as 'x' annotations as opposed to the 'o's of raw data points.  Various parameters set to NULL can supress different parts of the boxplot overlay (eg, boxol=NULL, wiskers=NULL) or alternatively other parameters can be passed via '...' directly to boxplot (as staplelty=0 and wisklty=0).  Aside from the normal passing of parameters to plot itself, many other parameters for the underlying plotting functions (boxplot, legend, axis, mtext) can also be passed in the following prefixed ways. For boxplot: boxplot.notch, boxplot.col, boxplot.border, boxplot.lty, boxplot.lwd work well (although default logic uses boxplot(range)=sparge(wiskers) (with defaults of wisker.lty=1, staple.lty=1) additionally with a pch of 'x' to mark outliers--but these can all be overidden. Nearly all of the legend params can be passed using the following names: legend.x, legend.y, legend.inset, legend.xjust, legend.border, legend.yjust, legend.box.col, legend.box.lwd legend.lty, legend.title, legend.title.col, legend.title.adj, legend legend=pt.bg, legend.cex, legend.ncol,  legent.text.col, legend.text.font. X and Y axis labels are currently handled via mtext where text and side are handled automatically by xlab & ylab inputs and line is determined by the first integer in par('mgp') [=axis(line)]), however, cex can becontroled by setting a value for "lab.cex". Likewise, the only way, currently, to customize the underlying call to 'axis()' is by assigning a value to axis.cex (but this also just defaults to par('cex').

lines.sparge() must be run after plot.sparge() and addess a series of lines corresponding to the groups plotted using x as data.frame and f as formula. 
}
\value{
a 'sparge' [sprinkler/smear] plot of point distributions
& the categorical variable dodge positions (only for the control variables) [invisble] 
}
\seealso{
See also 'boxplot' and 'stripchart' in package 'graphics' as well as 'sina', 'violin', 'bean', 'ridgelines', and 'raincloud' plots.
}
\examples{

print('RNORM EXAMPLE (f=NULL)')
x <- lapply(sample(1:5), function(avg) (rnorm(500,avg)))
names(x) <- letters[1:length(x)]
plot.sparge(x, pt.cols=rep('blue',length(x)), main='sparge plots:\nfor distributional comparison')

## four random distributrions (from the 'boxplot' examples)
print('BOXPLOT EXAMPLE DISTROS')
N.rds <- 1000
distros <- list(Uniform = runif(N.rds, min=.01, max=3), Normal = rnorm(N.rds), 
                                                         `T.df5` = rt(N.rds, df = 5), 
                                                      Gamma.s2 = rgamma(N.rds, shape = 2))
plot.sparge(distros, ylab='Distribution')
points(sapply(distros, median), 1:length(distros), col=gray(.6), pch="+", cex=2)
lines(sapply(distros, median), 1:length(distros), col='green')

# three more random distributions (from the 'sinaplot' examples)
print('MORE RANDOM DISTRIBUTIONS')
uniform <- runif(500, -4, 4)
bimodal <- c(rnorm(300, -2, 0.6), rnorm(300, 2, 0.6))
normal <- rnorm(800,0,3)
distributions <- list(uniform = uniform, bimodal = bimodal, normal = normal)
plot.sparge(distributions, ylab='more distributions', 
  boxplot.border='lightgreen', boxplot.lwd=2.3)


## using 'f' [as a factor] argument as grouping factor on just one treatment

# Orchard spray by treatment (compare with 'strip chart' plot)
print('ORCHARD')
OS <- with(OrchardSprays, split(decrease, treatment))
plot.sparge(OS, log = "x", main = "Orchard Sprays", xlab='Decrease',ylab='Treatment')

# Tooth Growth
print('TOOTH GROWTH 1')
plot.sparge(x=ToothGrowth$len, f=ToothGrowth$supp, xlab='Length', ylab='Supplement')


# multi-predictor using model-based parsing of 'f' [as a formula] and 'x' as a dataset

print('TOOTH GROWTH 2')
plot.sparge(x=ToothGrowth, f="len ~ dose",  horiz=FALSE, boxplot.notch=TRUE,
             ylab='Tooth Length', main='Tooth Growth')  #xlab='Dose')


print('TOOTH GROWTH 3')
plot.sparge(x=ToothGrowth, f="len ~ dose | supp", horiz=FALSE,
            ylab='Tooth length', main='Tooth Growth', xlab='Dose')

# CO2 dataset
print('CO2')
plot.sparge(CO2, 'uptake ~ Type | Treatment', horiz=FALSE,
            ylab='Uptake', main='CO2', xlab='Type', boxplot.lty=4)

# Joyner-Boore earthquake data (heavily rounded)
print('EARTHQUAKE')
attenu$magnitude <- as.factor(round(attenu$mag))
attenu$distance <- as.factor(round(log10(attenu$dist)))
plot.sparge(x=attenu, f="accel ~ distance | magnitude", horiz=FALSE, 
  xlab='log10(Distance)',ylab='Acceleration', main='Earthquake Attenuation', 
  boxplot.lwd=1.5, cpw=.7, boxplot.col=rgb(.8,.5,0,.15))

# Motor Trend cars data (rounded)
print('CARS')
mtcars$cylinders <- as.factor(mtcars$cyl)
plot.sparge(x=mtcars, f="qsec ~ gear | cylinders", horiz=FALSE, 
         xlab='# of Gears', ylab='Seconds', main='Motor Trend Cars',
         boxplot.lwd=2.3, boxplot.lty=2)


# NPK Yeilds
print('NPK YIELDS')
npk$N <- factor(npk$N, labels=c('<none>','Nitrogen'))
npk$P <- factor(npk$P, labels=c('<none>','Phosphorus'))
npk$K <- factor(npk$K, labels=c('<none>','Potasium'))
plot.sparge(npk, f="yield ~ N | K", xlab="Yield, pounds per (1/70) acre plot")

# Wool (warp breaks per loom)
print('WOOLOOM BREAKS')
plot.sparge(warpbreaks, f="breaks ~ wool | tension", boxplot.col=rgb(1, 1, 0, 0.15),
                    xlab="# of Breaks", ylab='Wool')

# US states
print('STATES')
states <- data.frame(region=state.region, area=state.area, division=state.division)
plot.sparge(x=states, f="area ~ region | division", ylab="Region", xlab='Area', cpw=1)

# Michelson Speed of Light Data
print('LIGHT')
morley$run <- cut(morley$Run, breaks=0:4*5)
plot.sparge(x=morley, f="Speed ~ Expt | run", main="Michelson Speed of Light Experiment",
            xlab='Speed',  ylab='Experiment', boxplot.lwd=1.4, boxplot.col=rgb(.7,.7,0,.2))

# Penguins
#plot.sparge(penguins, f="body_mass ~ island | species")

# Fertility  
print('FERTILITY')
infert$education <- as.factor(infert$education)
infert$ages <- jitter(infert$age, amount=1/2)
infert$spontaneous <- factor(infert$spontaneous, labels=c(0,1,2))
infertilty.model <- "ages ~ spontaneous | education "
pds <- plot.sparge(x=infert, f=infertilty.model, horiz=FALSE, cpw=.6, legend.inset=.05, 
       ylab='Ages, yrs (jittered)', xlab='# of Prior Miscarriages' , main='[in]Fertility', 
       boxplot.lty=2, boxplot.lwd=1.4, boxplot.col=rgb(.6,0,.4,.2), boxplot.varwidth=TRUE)
lines.sparge(x=infert, f=infertilty.model, pds=pds, horiz=FALSE, cat.order=2:1, pt.cex=3, lty=3)
lines.sparge(x=infert, f=infertilty.model, pds=pds, horiz=FALSE, cat.order=1:2, lwd=2, pt.cex=2)

}
