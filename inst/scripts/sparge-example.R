library(caroline)

data.path <- "data/forests/global.forest.watch/" 
# country level
cn.tc.drivers <- read.csv(paste(data.path,"cvs-exports/GFW-loss-country-treecover-drivers.csv",sep=''))
cn.pr.drivers <- read.csv(paste(data.path,"cvs-exports/GFW-loss-country-primary-drivers.csv",sep=''))
# sub national level
sn.pr.drivers <- read.csv(paste(data.path,"cvs-exports/GFW-loss-subnat-primary-drivers.csv",sep=''))

dl <- drivers.list <- list(alltrees.national=cn.tc.drivers, 
                           tropical.national=cn.pr.drivers, 
                           tropical.subnatnl=sn.pr.drivers)

for(df.nm in names(drivers.list)){
    print(df.nm)
    
    # recode the driver levels
    dl[[df.nm]] <- subset(dl[[df.nm]], driver!='Other natural disturbances') 
    dl[[df.nm]] <- subset(dl[[df.nm]], driver!='Settlements & Infrastructure')
    dl[[df.nm]]$driver[dl[[df.nm]]$driver=='Shifting cultivation'] <- 'Agriculture'
    dl[[df.nm]]$driver[dl[[df.nm]]$driver=='Permanent agriculture'] <- 'Agriculture'
    dl[[df.nm]]$driver[dl[[df.nm]]$driver=='Hard commodities'] <- 'Mining & Energy'

    # convert the loss.ha outcome to log & year & driver into factors  
    #dl[[df.nm]]$years <- cut(x=dl[[df.nm]]$year, breaks=2002+c(0:4*6)-1)
    dl[[df.nm]]$tc_loss_ha_log <- round(log(dl[[df.nm]]$tc_loss_ha +1), 2)
    dl[[df.nm]]$driver.f <- as.factor(dl[[df.nm]]$driver)

    # convert country and sub.national into factors as well
    if(df.nm == 'tropical.subnatnl'){  # sub-national only!
    dl[[df.nm]]$subnational.f <- as.factor(dl[[df.nm]]$subnational1)                                      }  # sub- national only!
    dl[[df.nm]]$country.f <- as.factor(dl[[df.nm]]$country)

    # get rid of unnecessary columns ('year' is now 'years' and 'loss' is not 'loss_log')
    dl[[df.nm]] <- dl[[df.nm]][        ,!names(dl[[df.nm]])%in%c('threshold','tc_loss_ha')] #,'year')]  #let's swap 'year' with years'
    
    # WRITE (accessory) LOOK-UP [LU] TABLES 
    if(df.nm == 'tropical.subnatnl'){  # sub-national only!
    write.csv(x=nv2df(nv(name=levels(dl[[df.nm]]$subnational.f)), col.names=c('id','subnational')),
     file=paste(data.path,"DB.id-style/GFW-loss-",df.nm,"-drivers-subnation.LU.csv", sep=""), row.names=F) } # sub-national only!
    write.csv(x=nv2df(nv(name=levels(dl[[df.nm]]$country.f)),col.names=c('id','country')), 
     file=paste(data.path,"DB.id-style/GFW-loss-",df.nm,"-drivers-country.LU.csv", sep=""), row.names=F) 
    if(df.nm == 'tropical.subnatnl'){  # sub-national only!
    write.csv(x=nv2df(nv(name=levels(dl[[df.nm]]$driver.f)),col.names=c('id','driver')), 
     file=paste(data.path,"DB.id-style/GFW-loss-driver.LU.csv", sep=""), row.names=F) }# only need 1 # sub-national only!

    ## convert each of the lookup table integer/id columns into new variable names and remove the factor predecessor variables
    if(df.nm == 'tropical.subnatnl'){  # sub-national only!
    dl[[df.nm]]$subnational.id <- as.numeric(dl[[df.nm]]$subnational.f); dl[[df.nm]]$subnational.f <- NULL} # sub-national only!
    dl[[df.nm]]$country.id <- as.numeric(dl[[df.nm]]$country.f); dl[[df.nm]]$country.f <-NULL
    dl[[df.nm]]$driver.id <- as.numeric(dl[[df.nm]]$driver.f); dl[[df.nm]]$driver.f <- NULL

    # WRITE (main) DATABASE [DB] TABLE 
    write.csv(dl[[df.nm]][!(names(dl[[df.nm]]) %in% c('subnational1','country', 'driver'))], 
     file=paste(data.path,"DB.id-style/GFW-loss-",df.nm,"-drivers.DB.csv", sep=""), row.names=F)

}


