## MODEL SPLITTING

.get.model.out <- function(model) {as.character(as.formula(model))[2]}
.get.model.args <- function(model) {strsplit(x=as.character(as.formula(model))[3],split=' *\\+ *')[[1]]}
#get.model.pred <- function(model) {strsplit(x=as.character(as.formula(model))[3],split=' *\\+ *')[[1]]}
## UNUSED
#.split.model.args <- function(formula.term.3) strsplit(formula.term.3, '[ *|]+')[[1]]  # assumes "|" delimiters
#.get.model.predictors <- function(formula.term.3){   

# the following alternative splits on "|"  <- as used in plot.sparge and plot.confound grid
.formula2parts <- function(formula){
    form <- as.formula(formula);    
    if( length(form) != 3){stop("formula is malformed")}
    form.parts <- as.character(form)
    form.preds <- strsplit(form.parts[3], '[ |]+')[[1]]
    return(list(outcome=form.parts[2], predictors=form.preds))
}



## formula testing
.is.formula <- function(form){ is.call(form) && form[[1]] == quote(`~`) }  # thx @moodymudskipper
.can.formula <- function(str){catch <- try(as.formula(str), silent=T); return(is.character(str) && (length(str)==1) && (class(catch)!='try-error'))}


## factor testing
.can.factor <- function(x){     fctch <- try(as.factor(x), silent=T);  actch <- try(as.character(x), silent=T)
 return((length(x)>1) && (class(fctch)!='try-error')         && (class(actch)!='try-error'))}


# TODO: model checks and validation w/ warnings and error calls




