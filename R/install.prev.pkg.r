
install.prev.pkg <- function(pkg.nm, version=NULL, repo.url='https://cran.r-project.org/src/contrib/Archive/'){
 
  package.location <- system.file(package=pkg.nm)
  if(nchar(package.location) > 0){
    stop(paste('Good news! Package',pkg.nm,'is INSTALLED in system at', package.location))
    #quit() # possible alternative instead of throwing an error with stop()
  }
  archive.page <- readLines(paste(repo.url,pkg.nm,sep=''))

  vers.regex <- "\\d+[\\.-]\\d+[\\.-]\\d+"
  pkg.regex <- paste(pkg.nm, "_",vers.regex,"\\.tar\\.gz", sep='')

  archive.links <- grep(x=archive.page, pattern=pkg.regex, value=T)
  archive.files <- m(archive.links, pattern=paste("(",pkg.regex,")",sep=''))
  archive.verss <- m(archive.files, pattern=paste("_(",vers.regex,")",sep='')) # recent addition of "_"
  n <- length(archive.files)

  if(is.null(version)){
    vers.2.use <- grep(archive.verss[n], pattern=vers.regex, value=T)
    warning(paste("didn't find a version using the second to last one:",vers.2.use))
  }else{
    vers.match <- grep(archive.verss, pattern=version) 
    if(length(vers.match)>1)
     stop('found too many matches for current package version') 
    vers.2.use <- archive.verss[vers.match-1]
  }

  pkg.file.2.use <- paste(pkg.nm,"_",vers.2.use,".tar.gz",sep='')

  install.packages(paste(repo.url, pkg.nm, pkg.file.2.use, sep='/'), dependencies=T)
  install.prev.pkg(pkg.nm=pkg.nm, version=vers.2.use, repo.url=repo.url)
    
}
