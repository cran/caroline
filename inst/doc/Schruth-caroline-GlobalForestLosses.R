### R code from vignette source 'Schruth-caroline-GlobalForestLosses.Rnw'

###################################################
### code chunk number 1: foo
###################################################
opt.old <- options(keep.source = TRUE, width = 95)
pkgD <- packageDescription("caroline")
vers <- pkgD$Version


###################################################
### code chunk number 2: downloadpackages
###################################################
# wget https://cran.r-project.org/src/contrib/Archive/caroline/caroline_1.0.1.tar.gz
# R CMD INSTALL caroline_1.0.1.tar.gz 


###################################################
### code chunk number 3: loadlib
###################################################
library(caroline)


###################################################
### code chunk number 4: read
###################################################
tl.outcome.var <- 'tc_loss_ha_log'  # the main "treeloss" outcome variable (log of total hectares lost) 
C.df <- read.csv(system.file("extdata",'countries.contients.csv', package="caroline")); C.df$X <- NULL;#, row.names=2)
c.dup <- table(C.df$country)>1; C.df <- subset(C.df, !country%in%names(c.dup)[c.dup]); rownames(C.df)<-C.df$country
continents <- nv(C.df, 2:1)
## Global Forest Watch downloaded datafiles   
#       national level
alltrees.loss.n  <- read.csv(system.file("extdata",'forest',"GFW-loss-alltrees.national-drivers.DB.csv", package="caroline")) # USE THIS (th=30) ONE INSTEAD
tropical.loss.n  <- read.csv(system.file("extdata",'forest',"GFW-loss-tropical.national-drivers.DB.csv", package="caroline")) # tropical compare to sn!
#   SUB-national level  (first level just be low nation: eg: state/province)
tropical.loss.sn <- read.csv(system.file("extdata",'forest',"GFW-loss-tropical.subnatnl-drivers.DB.csv", package="caroline")) 

## three corresponding (2-column) database-style lookup tables to reduce packaged/storage size of [SubNational] csv 
nats.alltre.lu <- read.csv(system.file("extdata",'forest',"GFW-loss-alltrees.national-drivers-country.LU.csv", package="caroline")) 
nats.tropic.lu <- read.csv(system.file("extdata",'forest',"GFW-loss-tropical.national-drivers-country.LU.csv", package="caroline")) 
nats.trp.sn.lu <- read.csv(system.file("extdata",'forest',"GFW-loss-tropical.subnatnl-drivers-country.LU.csv", package="caroline")) 
subnats.trp.lu <- read.csv(system.file("extdata",'forest',"GFW-loss-tropical.subnatnl-drivers-subnation.LU.csv", package="caroline")) 
drivers.lu     <- read.csv(system.file("extdata",'forest',"GFW-loss-driver.LU.csv", package="caroline"))  #same for all 

alltrees.loss.n <- nerge(all.x=T, method='lookup',## demo of the 'nerge()' and 'nv()' caroline package R functions
  l=list(all=alltrees.loss.n,  country=nv(nats.alltre.lu),
                               driver=nv(drivers.lu))) 

tropical.loss.n <- nerge(all.x=T, method='lookup',## demo of the 'nerge()' and 'nv()' caroline package R functions
  l=list(tl1=tropical.loss.n,  country=nv(nats.tropic.lu),
                               driver=nv(drivers.lu))) 

tropical.loss.sn <- nerge(all.x=T, method='lookup',## demo of the 'nerge()' and 'nv()' caroline package R functions
  l=list(tl2=tropical.loss.sn, country=nv(nats.trp.sn.lu),
                          subnational=nv(subnats.trp.lu), 
                               driver=nv(drivers.lu))) 

# an alternative piece-meal example of the above one-liner for both:#
#  1) using nv to create vectors from lookup tables: nv(<lookuptable.lu>, name='id')
#  2) merging the main table to these these three other lookup tables one at a time: nerge(list(df,vect)) 
#tropical.loss.sn.ex <- nerge(list(tl=tropical.loss.sn   , country=    nv(nats.trp.sn.lu, 'id')), by.x='ids', all.x=T) 
#tropical.loss.sn.ex <- nerge(list(tl=tropical.loss.sn.ex, subnational=nv(subnats.trp.lu, 'id')), by.x='ids', all.x=T) 
#tropical.loss.sn.ex <- nerge(list(tl=tropical.loss.sn.ex, driver=     nv(drivers.lu, 'id')), by.x='ids', all.x=T) 

## examples of recoding & conversions  used in another script but in the context of the data.reconfig code below 
## performed on the 'sub-national' dataset prior to saving as the above [GFW].csv file
# drivers.df <- subset(drivers.df, driver!='Other natural disturbances') 
# drivers.df <- subset(drivers.df, driver!='Settlements & Infrastructure')
# drivers.df$driver[drivers.df$driver=='Shifting cultivation'] <- 'Agriculture'
# drivers.df$driver[drivers.df$driver=='Permanent agriculture'] <- 'Agriculture'
# drivers.df$driver[drivers.df$driver=='Hard commodities'] <- 'Mining & Energy'


###################################################
### code chunk number 5: prepare.data.national
###################################################
##################
### NATIONAL level
###### ALL TREES ######
alltrees.loss.n.C <- merge(x=alltrees.loss.n, y=C.df, by='country', all.x=T) #merge tree loss data to continent dataset 
# turn most predictors in to factors
alltrees.loss.n.C$years     <-  cut(x=alltrees.loss.n.C$year, breaks=2002+c(0:4*6)-1)
alltrees.loss.n.C$continent <- factor(alltrees.loss.n.C$continent)
alltrees.loss.n.C$driver    <- factor(alltrees.loss.n.C$driver)

all.nations.TOTs.by.C <- tapply(X=alltrees.loss.n.C[,tl.outcome.var], INDEX=list(alltrees.loss.n.C$continent), FUN=sum)

all.nations.TOTs.by.c <- groupBy(df=alltrees.loss.n.C, by='country',aggregation=c('min','sum'), 
                                                                     clmns=c('country',tl.outcome.var))

alltrees.loss.n.XTonCd <- tapply(X=alltrees.loss.n.C[,tl.outcome.var],  FUN=sum,
                                                                INDEX=list(alltrees.loss.n.C$continent, 
                                                                           alltrees.loss.n.C$driver))
###### TROPICAL ######
tropical.loss.n.C <- merge(x=tropical.loss.n, y=C.df, by='country', all.x=T) ##merge tree loss data to continent dataset 
# turn most predictors in to factors
tropical.loss.n.C$years     <-  cut(x=tropical.loss.n.C$year, breaks=2002+c(0:4*6)-1)
tropical.loss.n.C$continent <- factor(tropical.loss.n.C$continent)
tropical.loss.n.C$driver    <- factor(tropical.loss.n.C$driver)

tropical.nations.TOTs.by.C <- tapply(X=tropical.loss.n.C[,tl.outcome.var], INDEX=list(tropical.loss.n.C$continent), FUN=sum)

tropical.loss.n.XTonCYs <- tapply(X=tropical.loss.n.C[,tl.outcome.var],  FUN=sum,
                                                                INDEX=list(tropical.loss.n.C$years, 
                                                                           tropical.loss.n.C$continent))
tropical.loss.n.XTonCd <- tapply(X=tropical.loss.n.C[,tl.outcome.var],  FUN=sum,
                                                                INDEX=list(tropical.loss.n.C$continent, 
                                                                           tropical.loss.n.C$driver))
tropical.nations.TOTs.by.yr  <- groupBy(tropical.loss.n.C, by='year', aggregation=c('max','sum'), clmns=c('year',tl.outcome.var), )
tropical.nations.TOTs.by.c <- groupBy(df=tropical.loss.n.C, by='country',aggregation=c('min','sum'), 
                                                                     clmns=c('country',tl.outcome.var))


###################################################
### code chunk number 6: prepare.data.subnational
###################################################
#########################
### SUB-NATIONAL level*
tropical.loss.sn.C <- merge(x=tropical.loss.sn, y=C.df, by='country', all.x=T) ##merge tree loss data to continent dataset 
# turn most predictors in to factors
tropical.loss.sn.C$years     <-  cut(x=tropical.loss.sn.C$year, breaks=2002+c(0:4*6)-1) # factorized
tropical.loss.sn.C$continent <- factor(tropical.loss.sn.C$continent)
tropical.loss.sn.C$driver    <- factor(tropical.loss.sn.C$driver)
tropical.loss.sn.C$tc_loss_ha_log.f <- cut(tropical.loss.sn.C[,tl.outcome.var], breaks=c(0,3,6,9,12,15)) 

tropical.subnats.TOTs.by.C <- tapply(X=tropical.loss.sn.C[,tl.outcome.var], INDEX=list(tropical.loss.sn.C$continent), FUN=sum)

tropical.subnats.XTbyc <- tab2df(table(tropical.loss.sn$country, tropical.loss.sn[,tl.outcome.var]), check.names=F)
tropical.subnats.XTbyc$total <- apply(tropical.subnats.XTbyc, 1, sum)

tropical.subnats.TOTs.by.yr  <- groupBy(tropical.loss.sn.C, by='year', aggregation=c('max','sum'), clmns=c('year',tl.outcome.var), )
tropical.subnats.TOTs.by.c  <- groupBy(tropical.loss.sn.C, by='country', aggregation=c('max','sum'), clmns=c('country',tl.outcome.var), )
tropical.subnats.TOTs.by.sn <- groupBy(tropical.loss.sn.C, by='subnational', aggregation=c('max','sum'), clmns=c('subnational',tl.outcome.var), )

tropical.subnats.SST.by.yd <- sstable(x=tropical.loss.sn.C, idx.clmns=c('year','driver'), ct.clmns=tl.outcome.var)
tropical.subnats.SST.by.Cd <- sstable(x=tropical.loss.sn.C, idx.clmns=c('continent','driver'), ct.clmns=tl.outcome.var)
tropical.subnats.SST.by.yC <- sstable(x=tropical.loss.sn.C, idx.clmns=c('year','continent'), ct.clmns=tl.outcome.var)

## cross-tabulate the particular sub-national regions with the most de-forestation
tropical.loss.sn.XTonCd <- tapply(X=tropical.loss.sn.C[,tl.outcome.var],  FUN=sum,
                                                                INDEX=list(tropical.loss.sn.C$continent, 
                                                                           tropical.loss.sn.C$driver))


###################################################
### code chunk number 7: all.types.continental.totals
###################################################
# printing out some summaries of the key variables of interest for each dataset
variables.of.primary.interest <- c('years','continent','driver',tl.outcome.var)
summary(alltrees.loss.n.C[ ,variables.of.primary.interest])
summary(tropical.loss.n.C[ ,variables.of.primary.interest])
summary(tropical.loss.sn.C[,variables.of.primary.interest])


###################################################
### code chunk number 8: all.types.continental.totals
###################################################
## ... also peek at the continent-level percentages for each 
pct(rev(sort(     all.nations.TOTs.by.C)));
pct(rev(sort(tropical.nations.TOTs.by.C)));
pct(rev(sort(tropical.subnats.TOTs.by.C)));


###################################################
### code chunk number 9: reporting.tabular.setup
###################################################
fp.lines <- 50 # number of lines for printing a full page of tabular data


###################################################
### code chunk number 10: all.nations.country.totals
###################################################
## we can also look a the nations with the most losses per dataset by their individual outcome measures
rev(sort(all.nations.TOTs.by.C))
print(head(all.nations.TOTs.by.c[rev(order(all.nations.TOTs.by.c[,tl.outcome.var])),], fp.lines/4), row.names=F)
rev(sort(all.nations.TOTs.by.C))
print(head(tropical.nations.TOTs.by.c[rev(order(tropical.nations.TOTs.by.c[,tl.outcome.var])),], fp.lines/4), row.names=F)
rev(sort(all.nations.TOTs.by.C))
print(head(tropical.subnats.TOTs.by.c[rev(order(tropical.subnats.TOTs.by.c[,tl.outcome.var])),], fp.lines/4), row.names=F)


###################################################
### code chunk number 11: combining.all.levels
###################################################
## Let's merge together all of the national totals for losses using the three different datasets'
all3dfs <- list(a.n=all.nations.TOTs.by.c, t.n=tropical.nations.TOTs.by.c, t.sn=tropical.subnats.TOTs.by.c)
## we want to first just extract a single 'loss,ha' column and name it by country
all3dfs.vl <- lapply(all3dfs, nv, name=c(tl.outcome.var, 'country'))
## we can then "nerge()" them together using my so-named name-merge function 
TOTs.by.c <- nerge(all3dfs.vl)#, method='rownames')
## we can now use the 'pct()' function to create column wise percentages to better compare across columns
PTCs.by.c <- pct(TOTs.by.c, clmns=names(TOTs.by.c)) 
PTCs.by.c <- PTCs.by.c[,grepl('pct',names(PTCs.by.c))] *100
PTCs.by.c$avg <- apply(PTCs.by.c[,1:2], 1, mean) # avg of just the first two 'national' level columns
## merge the finished percentage table back with the continents 
PTCs.by.cC<- nerge(list(pcts=round(PTCs.by.c,1), continents=continents)) #conts=C.df))
head(PTCs.by.cC[rev(order(PTCs.by.cC$avg)),], fp.lines*3/4)
## Above is a table of column-wise percentages of loss ordered by the "avg" column,
##  which is merely an average of the first two *national*level* columns. The third
##  *sub-national percentage column is merely here for informational/comparison reasons.


###################################################
### code chunk number 12: grouping.by.continent
###################################################
# now just look at the top best(inverse=T)->[worst] 5 countries in each continent
PTCs.by.cC$country <- rownames(PTCs.by.cC)
BB.PTC.cC <- bestBy(df=PTCs.by.cC, by='continents', best='avg', top=6, rebind=F, inverse=T)
print(BB.PTC.cC, row.names=F)
## Above is a version of of the forest loss percentage table that has been broken down by 
##  each continent, ranked within each sub section (using bestBy()) to highlight the top 
##  six highest forest losses for each country within in each continent separately.


###################################################
### code chunk number 13: country.driver.and.continent.by.years
###################################################
## Now let's look at how forest loss happens (for TROPICAL NATIONS ONLY) over time'
# we've already created a cross-tabulation of loss by continent over the years
tropical.loss.n.XTonCYs

# let's modify this to a finer level of partitioning so that the dirvers are included split by continents
tropical.loss.n.XTonYsdC <- tapply(X=tropical.loss.n.C[,tl.outcome.var],  FUN=sum,
                                                                INDEX=list(tropical.loss.n.C$years, 
                                                                           tropical.loss.n.C$driver,
                                                                           tropical.loss.n.C$continent))
tropical.loss.n.XTonYsdC
## the loss driver by years grouped by continent (tabular) format above can also be realized as 
##  a sparge plot showing all underlying datapoints (see the last plot [Fig.3] below for an example) 


###################################################
### code chunk number 14: reporting.plot.setup
###################################################
## plotting results defaults
axis.limit.outcome.an <- c(1, max(alltrees.loss.n[,tl.outcome.var])) # log(treeloss, ha)
axis.limit.outcome.tn <- c(1, max(tropical.loss.n[,tl.outcome.var])) # log(treeloss, ha)
axis.limit.outcome.sn <- c(1, max(tropical.loss.sn[,tl.outcome.var]))# log(treeloss, ha)
## last minue outcome and plot setup 
driver.cols <- nv(c('green','brown','purple','red'), levels(tropical.loss.sn.C$driver))
# main titles for the heat matrix plots
main.forest <- nv(c('All Trees (Worldwide)','Humid Tropical Primary Forest'), c('all', 'trp'))
main.level <- nv(c("(@ national level)",'(@ sub-national level)'), c('ntnl','subn'))
main.unit <- "loss in log(ha)"


###################################################
### code chunk number 15: heatmatrix.driver.by.continent
###################################################
###  HEATMATRIX PLOTS FOR ALL THREE DATASETS: [SUB-]NATIONAL & ALL/TROPICAL 
par(mfrow=c(3,1), mar=c(3,6,3,1), mgp=c(2.5,2,.5) )
heatmatrix(round(alltrees.loss.n.XTonCd ), text.col='chartreuse', cex=1.1, 
    main=paste(main.forest['all'], "\n", main.unit, main.level['ntnl']))
heatmatrix(round(tropical.loss.n.XTonCd ), text.col='chartreuse', cex=1.1, 
    main=paste(main.forest['trp'], "\n", main.unit, main.level['ntnl']))
heatmatrix(round(tropical.loss.sn.XTonCd), text.col='chartreuse', cex=1.1, 
    main=paste(main.forest['trp'], "\n", main.unit, main.level['subn']))


###################################################
### code chunk number 16: sparge.nation.by.country.driver
###################################################
###  SPARGE PLOTS FOR THE *NATIONAL* LEVEL
## NATIONAL LEVEL DATASET ##
# sparge plot: forest loss (@ national level): vs continent grouped by loss driver
par(las=1, cex=.7, mar=c(3,6,1,1), mgp=c(1.6,.7,0))
model.4 <- paste(tl.outcome.var, 'continent | driver', sep='~')
pds.4 <- plot.sparge(x=tropical.loss.n.C, f=model.4, xlim=axis.limit.outcome.tn,
            xlab='log(forest loss, ha)', ylab='', pt.cols=driver.cols, main='tropical nations',
            legend.cex=.6, legend.inset=.008, legend.title='loss driver',
            boxplot.notch=TRUE, boxplot.lwd=1.5, boxplot.col=rgb(0,0,1,.2))
lines.sparge(x=tropical.loss.n.C, f=model.4, pds=pds.4, cat.order=2:1, lwd=1.5, pt.cex=2,col=gray(.5)) 


###################################################
### code chunk number 17: sparge.nation.by.years.by.driver
###################################################
# sparge plot: forest loss (@ national level): vs years grouped by loss driver 
par(las=1, cex=.6, mar=c(3,3,1,1), mgp=c(1.6,.7,0)); horiz.5 <- FALSE
model.5 <- paste(tl.outcome.var, 'years | driver', sep='~'); 
pds.5 <- plot.sparge(x=tropical.loss.n.C, f=model.5, horiz=horiz.5, ylim=axis.limit.outcome.tn,
                   pt.cols=driver.cols, xlab='years', ylab='log(forest loss, ha)', las=1,
                      legend.cex=.7 , legend.inset=.112, legend.title='loss driver', main='tropical nations',
                         boxplot.notch=TRUE, boxplot.lwd=1.5, boxplot.col=rgb(0,0,1,.2))
## add some per-group median trendline overlays (using the returned p[oosition]d[odge]s above)
lines.sparge(x=tropical.loss.n.C, f=model.5, pds=pds.5, rb=driver.cols, lty=1, lwd=.8, horiz=horiz.5) 
lines.sparge(x=tropical.loss.n.C, f=model.5, pds=pds.5, cat.order=2:1, horiz=horiz.5, lwd=1.7, col=gray(.5))


###################################################
### code chunk number 18: heatmatrix1
###################################################
###  HEATMATRIX PLOTS FOR ALL THREE DATASETS: [SUB-]NATIONAL & ALL/TROPICAL 
par(mfrow=c(3,1), mar=c(3,6,3,1), mgp=c(2.5,2,.5) )
heatmatrix(round(alltrees.loss.n.XTonCd ), text.col='chartreuse', cex=1.1, 
    main=paste(main.forest['all'], "\n", main.unit, main.level['ntnl']))
heatmatrix(round(tropical.loss.n.XTonCd ), text.col='chartreuse', cex=1.1, 
    main=paste(main.forest['trp'], "\n", main.unit, main.level['ntnl']))
heatmatrix(round(tropical.loss.sn.XTonCd), text.col='chartreuse', cex=1.1, 
    main=paste(main.forest['trp'], "\n", main.unit, main.level['subn']))


###################################################
### code chunk number 19: sparge1
###################################################
###  SPARGE PLOTS FOR THE *NATIONAL* LEVEL
## NATIONAL LEVEL DATASET ##
# sparge plot: forest loss (@ national level): vs continent grouped by loss driver
par(las=1, cex=.7, mar=c(3,6,1,1), mgp=c(1.6,.7,0))
model.4 <- paste(tl.outcome.var, 'continent | driver', sep='~')
pds.4 <- plot.sparge(x=tropical.loss.n.C, f=model.4, xlim=axis.limit.outcome.tn,
            xlab='log(forest loss, ha)', ylab='', pt.cols=driver.cols, main='tropical nations',
            legend.cex=.6, legend.inset=.008, legend.title='loss driver',
            boxplot.notch=TRUE, boxplot.lwd=1.5, boxplot.col=rgb(0,0,1,.2))
lines.sparge(x=tropical.loss.n.C, f=model.4, pds=pds.4, cat.order=2:1, lwd=1.5, pt.cex=2,col=gray(.5)) 


###################################################
### code chunk number 20: sparge2
###################################################
# sparge plot: forest loss (@ national level): vs years grouped by loss driver 
par(las=1, cex=.6, mar=c(3,3,1,1), mgp=c(1.6,.7,0)); horiz.5 <- FALSE
model.5 <- paste(tl.outcome.var, 'years | driver', sep='~'); 
pds.5 <- plot.sparge(x=tropical.loss.n.C, f=model.5, horiz=horiz.5, ylim=axis.limit.outcome.tn,
                   pt.cols=driver.cols, xlab='years', ylab='log(forest loss, ha)', las=1,
                      legend.cex=.7 , legend.inset=.112, legend.title='loss driver', main='tropical nations',
                         boxplot.notch=TRUE, boxplot.lwd=1.5, boxplot.col=rgb(0,0,1,.2))
## add some per-group median trendline overlays (using the returned p[oosition]d[odge]s above)
lines.sparge(x=tropical.loss.n.C, f=model.5, pds=pds.5, rb=driver.cols, lty=1, lwd=.8, horiz=horiz.5) 
lines.sparge(x=tropical.loss.n.C, f=model.5, pds=pds.5, cat.order=2:1, horiz=horiz.5, lwd=1.7, col=gray(.5))


