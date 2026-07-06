.get.model.out <- function(model) {as.character(as.formula(model))[2]}
.get.model.args <- function(model) {strsplit(x=as.character(as.formula(model))[3],split=' *\\+ *')[[1]]}
#get.model.pred <- function(model) {strsplit(x=as.character(as.formula(model))[3],split=' *\\+ *')[[1]]}

## UNUSED
#.split.model.args <- function(formula.term.3) strsplit(formula.term.3, '[ *|]+')[[1]]  # assumes "|" delimiters

.is.formula <- function(form){ is.call(form) && form[[1]] == quote(`~`) }  # thx @moodymudskipper
.can.formula <- function(str){catch <- try(as.formula(str), silent=T); return(is.character(str) && (length(str)==1) && (class(catch)!='try-error'))}

.can.factor <- function(x){     fctch <- try(as.factor(x), silent=T);  actch <- try(as.character(x), silent=T)
 return((length(x)>1) && (class(fctch)!='try-error')         && (class(actch)!='try-error'))}


#.get.model.predictors <- function(formula.term.3){   

# TODO: model checks and validation w/ warnings and error calls




