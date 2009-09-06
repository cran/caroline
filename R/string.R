

pad <- function(vect,np){
 x <- as.numeric(vect)
 vect <- as.character(vect)
 for(p in 10^(1:np)){
   vect[x<p] <- paste("0",vect[x<p],sep="")
 }
 vect
}



m <- function(pattern, vect, names="V", types="character"){
  matches <- regexpr(pattern , vect )
  n <- length(gregexpr("[^\\]?)",pattern)[[1]]) #how many groups (non literal perends) does this pattern have
  if(n != length(names) | n != length(types)){
    print("ERROR: the number of backreferences in 'pattern' must equal the length of the 'names' and 'types' vectors")
  }else{
    ret <- list() 
    for(i in 1:n){
      this.vect <- as.vector(rep(NA,length(vect), mode=types[i]))
      this.vect[matches > 0] <- as.vector(sub(paste("(.*)", pattern ,"(.*)",sep=""),paste("\\",i+1,sep=""), vect[matches > 0]), mode=types[i]) 
      ret[[names[i]]] <- this.vect
    }
    if(n == 1)
      as.vector(ret[[1]])
    else
      as.data.frame(ret)
  }
}

