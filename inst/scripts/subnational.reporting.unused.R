


<<subsample.for.vector.plotting>>=
###   START PLOTS FOR THE *SUBNATIONAL* LEVEL
set.seed(68)
tropical.loss.sn.C.samp <- tropical.loss.sn.C[sample(1:nrow(tropical.loss.sn), size=round(nrow(tropical.loss.sn)*.068)),] 
@

<<sparge.subnat.vs.cont.by.driver>>=
## SUB-NATIONAL LEVEL DATASET (n=77,034 "sub-national" distrits) ##
# sparge plot: forest loss (@ sub-national level): vs continent grouped by loss driver
par(las=1, cex=.7, mar=c(3,7,1,1), mgp=c(1.6,.7,0))
model.4 <- paste(tl.outcome.var, 'continent | driver', sep='~')
pds.4 <- plot.sparge(x=tropical.loss.sn.C.samp, f=model.4, xlim=axis.limit.outcome.sn,
            xlab='log(forest loss, ha)', ylab='', pt.cols=driver.cols, main='sub-national/tropical',
            legend.cex=.65, legend.inset=.03, legend.title='loss driver',
            boxplot.notch=TRUE, boxplot.lwd=1.5, boxplot.col=rgb(0,0,1,.2))
lines.sparge(x=tropical.loss.sn.C.samp, f=model.4, pds=pds.4, cat.order=2:1, lwd=1.5, pt.cex=2,col=gray(.5)) 
@
\begin{figure}
\begin{center}
<<label=sparge.subnat.vs.cont.by.driver,fig=TRUE, echo=FALSE, results=hide, width=4, height=4>>=
<<sparge.subnat.vs.cont.by.driver>>
@
\end{center}
\caption{A sparge plot of forest loss (at the sub-national level): vs continent grouped by loss driver}
\label{plotiter:one}
\end{figure}
\clearpage
<<sparge.subnat.vs.years.by.driver>>=
# sparge plot: forest loss (@ sub-national level): vs years grouped by loss driver 
par(las=1, cex=.6, mar=c(3,3,1,1), mgp=c(1.6,.7,0))
model.5 <- paste(tl.outcome.var, 'years | driver', sep='~'); horiz.5 <- FALSE
pds.5 <- plot.sparge(x=tropical.loss.sn.C.samp, f=model.5, horiz=horiz.5, ylim=axis.limit.outcome.sn,
                   pt.cols=driver.cols, xlab='years', ylab='log(forest loss, ha)', las=1,
                      legend.cex=.7 , legend.inset=.13, legend.title='loss driver', main='sub-national/tropical',
                         boxplot.notch=TRUE, boxplot.lwd=1.5, boxplot.col=rgb(0,0,1,.2))
## add some per-group median trendline overlays (using the returned p[oosition]d[odge]s above)
lines.sparge(x=tropical.loss.sn.C, f=model.5, pds=pds.5, rb=driver.cols, lty=1, lwd=1.5, horiz=horiz.5) 
lines.sparge(x=tropical.loss.sn.C.samp, f=model.5, pds=pds.5, rb=driver.cols, lty=2, lwd=.8, horiz=horiz.5) 
lines.sparge(x=tropical.loss.sn.C.samp, f=model.5, pds=pds.5, cat.order=2:1, horiz=horiz.5, lwd=1.7, col=gray(.5))
@
\begin{figure}
\begin{center}
<<label=sparge.subnat.vs.years.by.driver,fig=TRUE,echo=FALSE, results=hide, width=4, height=4.5>>=
<<sparge.subnat.vs.years.by.driver>>
@
\end{center}
\caption{A [vertical] sparge plot of forest loss (at the sub-national level): vs years grouped by loss driver. The medians ("+") of each box plot for each subgroup have been externally joined together via dashed lines showing increasing trends for our four primary drivers of forest loss. Note that solid vs dashed lines reflect the full vs sampled datasets (to slim this already large vector-plot to a packagable size) }
\label{test1:one}
\end{figure}
\clearpage

<<subnational.totals>>=
## .... even more tabular results 
# [sub-national*] tree loss summed by country --- grouped at (integer log(hectares) ranges]
#head(tropical.subnats.TOTs.by.c[rev(order(tropical.subnats.TOTs.by.c[,tl.outcome.var])),]) #tropical.subnats.XTbyc?
head(rev(sort(tropical.subnats.TOTs.by.C)), fp.lines)
# Note that although the countries of South East Asia & Oceania dominate in total median loss, 
# the most extreme loss outlier states/provinces reside in Brazil & Kalimantan (Indonesian Borneo)
#(*European data not available at this level!
@ 
\clearpage




<<linear.models>>=

## linear regression models at the national and sub-national levels
# NATIONAL  (no effect for individual years)
alltrees.loss.n.C$russia <- alltrees.loss.n.C$country == 'Russia'
alltrees.loss.n.C$canada <- alltrees.loss.n.C$country == 'Canada'
alltrees.loss.n.C$brazil <- alltrees.loss.n.C$country == 'Brazil'
lm.c <- lm(data=alltrees.loss.n.C, paste(tl.outcome.var,'continent + russia + canada + brazil',sep='~'))
summary(lm.c)
## SUB-NATIONAL
lm.sn <- lm(data=tropical.loss.sn.C, paste(tl.outcome.var,'years + continent + driver', sep='~'))
summary(lm.sn)
@ 

