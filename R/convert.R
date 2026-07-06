


## table to dataframe (with names!)
tab2df <- function(x,...){

  is.tabrix <- is.table(x) | is.matrix(x) #class(x)[1] %in% c('table','matrix')
  row.nms <- rownames(x)
  
  if(length(dim(x))>1){ #NCOL(x)>1){
    clm.nms <- colnames(x)
    if(is.tabrix)
      x <- lapply(clm.nms, function(i) x[,i]) 
  }else{ # is a vector                                     
    if(is.tabrix){ # single column but matrix output
      clm.nms <- row.nms  # assume we want a 2 clmn dataframe
      row.nms <- NULL
    }else{   # vectors
      clm.nms <- NULL
      if(!is.null(names(x))) #named vector
        row.nms <- names(x) 
      else    # unnamed vector
        row.nms <- 1:length(x)
   }
  }
  
  if(!is.null(clm.nms)){
    x <- as.list(x)
    names(x) <- clm.nms
  }
  df <- data.frame(x,...) #check.names=FALSE?
  rownames(df) <- row.nms
  return(df)
  
}




nv <- function(x=seq(along=name), name=letters[length(x)], key.clmn='id'){

  if(is.data.frame(x)){
    if(NCOL(x)==2 && (key.clmn %in% names(x))){
      message(paste(c("found key clmn '", key.clmn,"' in a two clmn (DB lookup?) table, using it to name the other clmn"), collapse=''))
      data.clmn <- names(x)[!names(x) %in% key.clmn]
      rownames(x) <- name <- x[,key.clmn] # rownames gets used to name the vector below
    }else{
      message(paste(c("using the first element of 'name' ('",name[1],"') to extract data from x"),collapse=''))
      data.clmn <- name[1]
    }
    v <- x[,data.clmn]
    if(length(name)==2){
      names(v) <- x[,name[2]]
    }else{
      names(v) <- rownames(x)
    }
  }else{
    if(NCOL(x)!=1)
      stop('x must be unidimentional (if not a dataframe)')
    v <- x
    if(length(x) != length(name)) stop("'x' and 'name' must have the same length  for a unidimentional 'x'")
    names(v) <- name
  }
  v
}

nv2df <- function(x, clmn.names=c('x','name'), ...){
   df <- data.frame(x=x, name=names(x), ...)
   names(df) <- clmn.names 
   df
}


pct <- function(x, clmns=NA, digits=2, suffix='%'){

   if(is.data.frame(x)){
    for(clmn in clmns)
     x[,paste(clmn,'pct',sep='.')] <- x[,clmn]/sum(x[,clmn])
   }else{
     x.names <- NULL
     x <- x/sum(x)
     if(!is.numeric(digits)) stop('"digits" must be an integer specifying the number of decimal places for rounding')
     if(!is.null(names(x))) x.names <- names(x)
     x <- round(x, as.integer(digits))
     if(suffix=='%')
       x <- paste(x*100,suffix, sep='')
     if(!is.null(x.names)) names(x) <- x.names 
   }
  return(x) 
}

rerowname <- function(df, old='NA', new ='unknown'){
  tmp.rn <- rownames(df)
  tmp.rn[tmp.rn==old] <- new
  rownames(df) <- tmp.rn
  df
}
