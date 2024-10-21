
.clean.report.string <- function(string){   
  for(pat in c(c('T\\(','TR\\(','TRU\\(','TRUE\\('),'\\..*\\(')){ ### LAST ONE ("\\.") IS SPECIFIC TO MY MODELS -> GENERALIZE?
    string <- gsub(x=string, pattern=pat, replacement='(') }
  return(string)
}

fit.1ln.rprt <- function(fit, method=c('std.dev','p-value')[1], decimal.places=3, name.char.len=6, print.inline=TRUE, rtrn.line=FALSE, R2=TRUE,mn=''){
  ps <- summary(fit)$coefficients[-1,4]
  ts <- summary(fit)$coefficients[-1,3]
  R2adj<- round(summary(fit)$adj.r.squared,3)
  #AICc <- round(correct.AIC(AIC=AIC(fit),K=fit$k, n=fit$n), 2)
  coefs <- coefficients(fit)[-1]
  if(method=='p-values'){
   sigs <- sapply(-ceiling(log(ps,10)), function(t) paste(rep('+',min(t,3)),collapse=''))
  }else{
   sigs <- sapply(floor(abs(ts)), function(t) paste(rep('+',min(t,3)),collapse=''))
  }
  sigs[coefs<0] <- gsub(x=sigs[coefs<0],pattern='\\+','-') #seems backwards...
  names(ps) <- paste(sigs,names(coefs),sep='')
  names(ps) <- substr(names(ps),1,name.char.len)
  coefs.pos <- coefs[coefs>0]
     ps.pos <-    ps[coefs>0]
  coefs.neg <- coefs[coefs<0]
     ps.neg <-    ps[coefs<0]

  ps.p.ord <- order(ps.pos); 
  ps.n.ord <- rev(order(ps.neg))
  out.vect <- round(c(ps.pos[ps.p.ord], ps.neg[ps.n.ord]),decimal.places)
  # consider cleaning up names here so that anything after a "." is removed (as well as "TRUE")

    .paste.pvals2prnds <- function(ov){ paste(names(ov),paste("(",ov,")",sep=''),sep='')}
    report.pos <- .paste.pvals2prnds(out.vect[                 1: length(ps.pos)                ]); if(length(ps.pos)==0){report.pos <-''}
    report.neg <- .paste.pvals2prnds(out.vect[(length(ps.pos)+1):(length(ps.pos)+length(ps.neg))]); if(length(ps.neg)==0){report.neg <-''}
    out.line <- c(report.pos," | ",report.neg)
    outline <- paste(.clean.report.string(out.line),collapse=' ')
    if(R2){outline <- paste(outline, 'R2adj:',R2adj)} #,'AICc:',AICc)}
  if(print.inline){
    cat(paste(mn,outline,'\n'))#print(, quote=FALSE)
  }
  if(rtrn.line)
   invisible(outline)
  else 
   invisible(out.vect)
}


