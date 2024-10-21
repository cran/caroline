.get.model.out <- function(model) {as.character(as.formula(model))[2]}
.get.model.args <- function(model) {strsplit(x=as.character(as.formula(model))[3],split=' *\\+ *')[[1]]}

.split.model.args <- function(formula.term.3) strsplit(formula.term.3, '[ *|]+')[[1]]

#.get.model.predictors <- function(formula.term.3){   

# TODO: model checks and validation w/ warnings and error calls




