# DYNAMIC MODEL -> AXES LABELING
.model.vars.2.xy.labs <- function(plot.orient.horiz, invar, outvar, plot.default.list=list(xlab='', ylab='', xaxt='n', yaxt='n')){
    if(plot.orient.horiz){plot.default.list$xlab <- outvar; plot.default.list$ylab <- invar  
                    }else{plot.default.list$xlab <- invar ; plot.default.list$ylab <- outvar }
    return(plot.default.list)
}
.model.f.2.xy.labs <- function(plot.orient.horiz, formula, plot.default.list){#}= list(xlab='', ylab='', xaxt='n', yaxt='n')){
    formparts <- .formula2parts(formula)
    in1 <- formparts$predictors[1] # main predictor ("formula predictor #1")# outcome variable name
    out <- formparts$outcome ;  # outcome variable
    new.plot.defaults <- .model.vars.2.xy.labs(plot.orient.horiz, invar=in1, outvar=out, plot.default.list)
    return(new.plot.defaults)
}

## MODEL SPLITTING
.formula2parts <- function(formula){
    form <- as.formula(formula);    
    if( length(form) != 3){stop("formula is malformed")}
    form.parts <- as.character(form)
    form.preds <- strsplit(form.parts[3], '[ |]+')[[1]]
    return(list(outcome=form.parts[2], predictors=form.preds))
}

.sparge.plot.sub.functions <- c('legend', 'boxplot', 'axis', 'mtext') # other plotting functions used in wrapper
.sf.OR.pattern <- function(except='', function.vector=.sparge.plot.sub.functions){ 
    if(!is.na(except)){function.vector <- function.vector[(!function.vector==except)]}
    paste("^(", paste(function.vector,collapse='|'),').',sep='')
}


# new function, yet to be used, but should be several times above to eliminate redundancies
.parse.params.pass.parts <- .pppp <- function(defaults, dots, reserved, sub.funcs, this.func=NA, unprefix=paste(this.func,'.',sep="")){
         p.viauser <- dots[!(dots%in%reserved) & !grepl(x=names(dots), pattern=.sf.OR.pattern(except=this.func, function.vector=sub.funcs))]
         if(!is.na(this.func)){  #'this.func' is the name of a sub function within the wraper function (plot.sparge), otherwise no un-prefixing
         p.viauser <- p.viauser[grepl(x=names(p.viauser), pattern=paste('^',unprefix,sep=''))]
   names(p.viauser) <- sub(pattern=unprefix, replacement='', x=names(p.viauser))} 
         p  <- c(defaults[!names(defaults) %in% names(p.viauser)], p.viauser)
  return(p)
}


plot.sparge <- function(x, f=NULL, cat.names=if(is.factor(f)){levels(f)}else{NULL}, out.range=range(unlist(x)),  cpd=0, cpw=.4, jit.f=1, horiz=TRUE, add=FALSE, lgnd='auto', zl=FALSE,  pt.cols=1, boxol='gray',alpha=.2, wiskers=1.5, ...){
    # formula_1 = 'outcome~ predictor1.fp1 | control.fp2" # example
    # strsplit(strsplit(formula_1,'~')[[1]][2], "[|]")[[1]][1]
    verbose <- FALSE
    recursion <- NA # what level/percent have we recursed this function

    if(verbose) print('BEGIN plot.sparge()')

    ## HANDLE ALL "GLOBALS" HERE  # suppressed for initial boxplot && [recursive] plot.sparge call[s]


    # SET UP PLOT DEFAULTS {AXIS LABELS AND TICK_MARK LABELS) HERE (assuming formula-based option [1] just below)    
    PAG <- plot.axes.globals <- PAG.i <- list(xlab='', ylab='', xaxt='n', yaxt='n') # 
    axis.labs.nm.vect <- names(PAG[c('xlab','ylab')]) # ((built in sanity check))   # unused elsehwere!
    DOTS <- DOTS.i <- list(...)  # alternative option: using MC.INIT <- match.call(expand.dots = T)  
    if(verbose){print("DOTS.i (@init):"); print(DOTS.i)}
    
    #  CORRECT USER INPUT (esp. cat.names, f, and x) USING THE FOLLOWING IF/ELSE LOGIC 
    if(!is.null(f)){ #} && is.character(f) && (length(f)=1))   #  #   f <- as.formula(f)    
        if(.can.factor(f) && !is.factor(f)){ f <- as.factor(f)}
        if(.can.formula(f))                { f <- as.formula(f) }
    }
    if(is.null(cat.names)){   # handlng the data.frame/formula cases where cat names is not easily retrieved from a bonefide "factor"
        if( is.data.frame(x)){       #  OPTION #1
            if(.is.formula(f)){ cat.names <- as.character(unique(x[,.formula2parts(f)$predictors[1]])) }
        }else{  #  not a dataframe
            if(!is.list(x)){         # OPTION #2
               if(is.null(f)) stop('if x is neither a dataframe nor list, it must be numeric vector with f as a factor of equal length')
               if(is.factor(f)){  cat.names <- levels(f) }
            }else{                   # OPTION #3
                if(!is.null(names(x))){ cat.names <- names(x)}else{ cat.names <- 1:length(x)}
            }
        }
    }
    category.limit <- 10  # also used for the "control" sub-factor (fp2)
    if(length(cat.names) > category.limit )
        {warning(paste("probably too many levels in category names, ideally construct inputs resulting in fewer than", category.limit, 'categories'))}
    pds <- cats <- nv(seq(along=cat.names), cat.names)  # integers for predictor axis [pds is the globally RETURNED value <- plot.sparge()]

    axis.label.cat.side <- (1:2)[horiz+1] #corresponds to default of horiz = T
    axis.label.cnt.side <- (2:1)[horiz+1] #corresponds to default of horiz = T

    if('recursion' %in% names(DOTS.i)) ## WORKING THROUGH RECURSION CALLS to SPARGE
        recursion <- DOTS.i$recursion  #    1 > fp2levs.i/length(fp2.levs)
      #  if(recursion == 1){ # FINISHED CALLING SPARGE RECURSIVELY   100% = 1 = fp2levs.i/length(fp2.levs)

    if(is.data.frame(x) && (!.is.formula(f)))
        warning("x is a dataframe without a formula and could get treated like a ordinary list instead")

    ## OPTION 1: INPUT AS FORMULA & DATA ###########################  ###################### ###################### ######################
    if(is.data.frame(x) && (.is.formula(f))){  ##  [x as] df & [f]ormula ----->  [x as] vector(s) & [f]actor vector(s)
        PAG <- .model.f.2.xy.labs(horiz, formula=f, plot.default.list=PAG)  # based on formula input
        if(verbose) print('==OPTION 1==')

        if(verbose){print("PAG (#pre-recursion)"); print(PAG )}
        formula.parts <- .formula2parts(f)

        #formula.preds <- .frmprts2preds(f.p=formula.parts)
        on0 <- outcome.name   <- formula.parts$outcome; # outcome variable name
        fp1 <- main.predictor <- formula.parts$predictors[1] ; # main predictor ("formula predictor #1")
 
        ## OPTION 1a: CONVERT DATA (x[df] & f[formula] =>  x[vect] f=[vect])  IE: to send to OPTION 2 far below   ######################  
        if( length(formula.parts$predictors)==1 ){   # NUMBER OF PREDICTORS == 1   ( no recursion !!!!!!!!!!!!!!!)
            f <- x[,main.predictor] #must be run before next line (before x is changed to a vector)
            x <- x[,outcome.name];  # vectorize x (replace x as dataframe it with just one column of itself, the new x)
            if(!is.factor(f)) {f <- factor(f, labels=names(cats)) }# [f]actor from [f]ormula selected dataframe vector      # levels=names(cats)
         # qualifies for "both x & f inputs are vectors"   (SKIP TO OPTION 2)

        }else{                                       # NUMBER OF PREDICTORS == 2   (recursive call to plot sub-groups)
        ## OPTION 1b: INPUT AS FORMULA & DATA recursive call to list input IE: to send to OPTION 3 far below  ######################  
        ## this code chuunk is only run when recursions = 0 
        #if(length(formula.parts$predictors) == 2)
            #PAG$xlab & PAG$ylab <- ""  # this avoids plotting over and over again during recursion and creating fuzzy/bolded plot labels
            recursion <- 0
            if(length(formula.parts$predictors) > 2)
                {stop("only two predictor terms currently allowed in formula 'f'")}
            fp2 <- formula.parts$predictors[2] ; # control factor
            # TODO: add check so that xlab & ylab match terms (at least somewhat)
            if(!is.factor(x[,fp1])){
                {message("term before '|' should be a factor, coercing")}
                x[,fp1] <- as.factor(x[,fp1])  # factor is used to split the dataset before sending list of x-vectors to OPTION 3 recursively
            }  # x[,fp1] becomes f as factor in OPTION 2 
            #fp1.levs <- levels(x[,fp1])  # handled outside of this OPTION 1 by OPTION 3 now
            if(!is.factor(x[,fp2]))
                {message("term after '|' must be a factor, coercing")}
            x[,fp2] <- as.factor(x[,fp2])
            fp2.levs <- levels(x[,fp2])
            fp2.l.ct <- length(fp2.levs)
            if(fp2.l.ct  > category.limit) #category.limit ~ 10
                {warning(paste("probably too many levels in the 2nd predictor factor: [fp2], ideally use fewer than", category.limit))}
            if(length(pt.cols)==fp2.l.ct){
                if(is.null(names(pt.cols))) stop("'pt.cols' must be a named vector & match factor levels: try 'nv()'")
                lev.cols <- pt.cols
            }else{
                lev.cols <- nv(rainbow(fp2.l.ct), fp2.levs)
            }  # next line assumes that cpw is the original (eg.4) undivided version
            #if(!horiz){mlt.sgn <- 1}else{mlt.sgn <- -1} #vert reads left[-] to right[+] & horizontal reads top[+] to bottom[-] (via double negative)
            mlt.sgn <- ifelse(test=!horiz, yes=1, no=-1) #vert reads left[-] to right[+] & horizontal reads top[+] to bottom[-] (via double negative)
            pds <- nv(seq(from=-mlt.sgn*cpw/2, to=mlt.sgn*cpw/2, along.with=fp2.levs), fp2.levs) # (RET VAL of sparge()) #a function of cpw? #*fp2.l.ct/2
            news <- nv(c(F,rep(T, fp2.l.ct -1)), fp2.levs)  #  vector of "par(new=T/F)" as passed through recursive sparge(add=news[]) calls

            for(fp2lev.i in seq(along=fp2.levs)){
              fp2lev <- fp2.levs[fp2lev.i]
              recurs.pct <- fp2lev.i/length(fp2.levs)
               if(verbose){print("PAG (recursion loop)"); print(PAG )}
               if(verbose){print("DOTS.i (recursion loop)"); print(DOTS.i )}
              x.sub <- subset(x, x[,fp2]==fp2lev)
              x.list <- split(x.sub[ , eval(outcome.name)], x.sub[, eval(fp1)]) 
              # RECURISVE CALL (just down one level each loop iteration) TO PLOT.SPARGE
              #dots.list <- if(recurs.pct!=1){ DOTS.i}else{ PAG}  ## not sure how to get this to play nice with plain-ol' "..." below, to lab redundancy
              plot.sparge(x.list, out.range=range(x[,outcome.name]), # cat.names=fp1.levs,   
                          pt.cols = lev.cols[fp2lev], cpd=pds[fp2lev], add=news[fp2lev], cpw=cpw/fp2.l.ct, #xaxt=axts[fp2lev], axt=axts[fp2lev],
                           horiz=horiz, zl=zl, jit.f=jit.f, boxol=boxol, alpha=alpha, wiskers=wiskers, 
                           recursion=recurs.pct, ...)  # should be employing some kind of logic using PAG & DOTS.i (the use of "..." is temporary)
            } # end [recursive] looping through sub-lists of x (via fp2's factor levels)
            if(lgnd=='auto'){    # THIS CHUNK GETS RUN LAST FOR THE ENTIRE FUNCTION (... for "f= predict | CONTROL" calls only, obviously)
                if( horiz){low.dense.lgnd.pos <- legend.position( x[,outcome.name], as.numeric(x[,fp1]) )}
                if(!horiz){low.dense.lgnd.pos <- legend.position( as.numeric(x[,fp1]), x[,outcome.name] )} 
                lgnd.res <- c('legend','recursion')
                lgnd.defaults <- list(x=low.dense.lgnd.pos, legend=fp2.levs, pt.bg=lev.cols, pch=21, inset=.1, title=fp2) #col='gray'
                #dots.list  <- DOTS.i[!names(DOTS.i) %in% c('legend','recursion') &                  # input not2 trump 'x', 'legend
                #                      !grepl(x=names(DOTS.i), pattern=.sf.OR.pattern(except='legend'))] # ... or any other sub.function params
                #lgnd.viauser <- dots.list[grepl(x=names(dots.list), pattern='^legend.')]; 
          #names(lgnd.viauser) <- sub(pattern='legend.', replacement='', x=names(lgnd.viauser)) 
                lgnd.defsuser <- .pppp(defaults=lgnd.defaults, dots=DOTS.i, reserved=lgnd.res, sub.funcs=.sparge.plot.sub.functions, this.func='legend')
                do.call("legend",c(lgnd.defsuser))#lgnd.defaults[!names(lgnd.defaults) %in% names(lgnd.viauser)], )) # new version (yay "legend.[params]")
                #legend(x=low.dense.lgnd.pos, legend=fp2.levs, pt.bg=lev.cols, pch=21, col='gray',inset=.1, title=fp2)# old version (no "legend.[params]")
            x <- NULL ; f <- NULL  # facilitates SKIPPING SECOND HALF of this script (it runs anyway (SKIPPING TO OPTION 3) but only at recursive level)
            } # end if fp2 exists (two predictors) 
        } # if /else [ via fp1 vs fp2]  both x & f are NOT NULL  in top case send to option 2
    } # (end formula input option [potentially recursive call])    1st HALF of this FUNCTION  ###################### ######################

    ######################################################### INTERMISSION #########################################################
 
   if( !(is.null(x) && is.null(f)) ){   # handles exit from [recursive] FORMULA option above (brackets 2nd HALF of this FUNCTION!)  
 
    ## OPTION 2:  INPUT AS VECTORS (outcome + factor ONLY) -----> converts inputs to OPTION 3 format #############
    if(!is.list(x) && is.factor(f)){   ##  [x as] vector & [f]actor --->  [x as] list of numeric vectors
    if(verbose) print('==OPTION 2==')
        if(length(x) != length(f))
          {stop("for the 'plot sparge by vector' method factor f must be same length as vector x")}
        x <- split(x,f)    # The simplest (eg, "outcome ~ predictor") models end up here after conversion above in OPTION 1
        if(!all(levels(f)==names(cats)))
            stop('the levels of f should be the same as cat names')
        # qualifies for "input as list [of vectors]" #
        f <- NULL
        recursion <- NA
    } #(end vector input (OPTION #2) conversion)
    ##  FINISHED CONVERTING TO OPTION 3 FRIENDLY FORMAT


    ## OPTION 3: [default] INPUT AS DATAFRAME + FORMULA ######(case for finally plotting the sparge points & boxplots for the recursive calls above)
    ## ALL EVENTUALLY BECOME THIS OPTION (x=list & f=NULL), FROM EITHER OF OTHER TWO{ 1) x=DF + f=formula & 2) x=a.vector + f=a.factor [equal lengths]}
    if(is.list(x) & is.null(f)){   #[x as] list of numeric vectors -----> sparge plot
    if(verbose) print('==OPTION 3==')
       if(!(is.list(x) && all(sapply(x,is.numeric))))
         {stop("x should (by now) be a list (split-up by f?) of soley numeric vectors!)")}
          
       if(length(pt.cols)==1){ pt.cols <- rep(pt.cols,length(x))}  # no sub-grouping so everything is ()repeated as) the same color
       # convert to transparent colors
       pt.cols <- sapply(pt.cols, function(cl){ RGB <- col2rgb(cl)/255; rgb(RGB[1,],RGB[2,],RGB[3,], alpha=alpha)})

       if(is.null(names(x)))
         {stop('x must be a named list')}
       
       if(cpw > .5) stop("'cpw' should not be greater than .5 (plotted points of [integer spaced] categories will overlap!)")

       pred.pos.range <- c(min(cats)-abs(cpw), max(cats)+abs(cpw))   # used for (categorical) plot limits (xlim or ylim)
      #pred.positions <- lapply(1:length(x), function(pp) jitter(rep(pp,length(x[[names(x)[pp]]])), factor=jit.f, amount=cpw/2) + cpd)   ### !!!!!!!!
       pred.positions <- lapply(names(x), function(pn) jitter(rep(cats[pn], length(x[[pn]])), factor=jit.f, amount=cpw/2) + cpd)   ### !!!!!!!!

       cat.axis.range.spreader <- .4
       pred.pos.range <- pred.pos.range + (c(-1,1) * cat.axis.range.spreader)  # plot-edges catgory-group buffer (may also use: par(bty='n', xpd=T))

       if(horiz==TRUE){ xs <- unlist(x)             ; ys <- unlist(pred.positions); xlim <- out.range; ylim <- pred.pos.range
       }else{           xs <- unlist(pred.positions); ys <- unlist(x);              xlim <- pred.pos.range; ylim <- out.range
       } 
       par(new=add)
       xy.defaults <- append(list(xs, ys, ylim=ylim, xlim=xlim, pch=21, bg=pt.cols), plot.axes.globals); xy.res <- c('xs','ys','recursion')
       #xy.viauser <- DOTS.i[!(names(DOTS.i) %in% c('xs','ys','recursion')) &     # input not to trump 'xs', 'ys', recursion etc
       #                    !grepl(x=names(DOTS.i), pattern=.sf.OR.pattern())]  # ... or any other sub.function params
       #xy.viauser <- c(PAG.i[!names(PAG.i) %in% names(xy.viauser)], xy.viauser)  #  user specified DOTS will trump PAG default/updated values
       xy.defsuser <- .pppp(defaults=xy.defaults, dots=DOTS.i, reserved=xy.res, sub.funcs=.sparge.plot.sub.functions, this.func=NA)
       #do.call("plot",c(xy.defaults[!names(xy.defaults) %in% names(xy.viauser)], xy.viauser))#xy.defsuser  
       do.call("plot",xy.defsuser)   ##### SPARGE POINT-SWATHS PRODUCED HERE #####
       #plot(xs, ys, ylim=ylim, xlim=xlim, xaxt=xxt, yaxt=yxt, pch=21, bg=pt.cols, ...)   # old version

       gray.scale <- gray(pgeom(length(xs), prob=.02) -.01)
       if(!is.null(boxol)){
        if(boxol=='gray'){ bc = gray.scale }else{ bc=boxol}  
        if(is.null(wiskers)){wl = 0; sl = 0}else{wl = 2; sl = 1}
        bp.defaults <- append(list(x=x, at=1:length(x)+cpd, horizontal=horiz, col='transparent', add=TRUE,  #las=1,  varwidth=T, 
                              border=bc, boxwex=cpw, range=wiskers, outline=TRUE, pch='x', whisklty=wl, staplelty=sl), 
                       plot.axes.globals) ; bp.res <- c('x','f', names(PAG), 'recursion')
        #dots.list <- DOTS.i[!(names(DOTS.i) %in% c('x','f', names(PAG), 'recursion')) &        # input not to trump 'x', 'f', recursion etc
        #                    !grepl(x=names(DOTS.i), pattern=.sf.OR.pattern(except='boxplot'))] # ... or any other sub.function params
        #bp.viauser <- dots.list[grepl(x=names(dots.list), pattern='^boxplot.')]; 
  #names(bp.viauser) <- sub(pattern='boxplot.', replacement='', x=names(bp.viauser))       # could also just pass via boxplot(pars=par())?? 
        bp.defsuser <- .pppp(defaults=bp.defaults, dots=DOTS.i, reserved=bp.res, sub.funcs=.sparge.plot.sub.functions, this.func='boxplot')
        do.call("boxplot",c(bp.defsuser)) #bp.defaults[!names(bp.defaults) %in% names(bp.viauser)], ))  #####   BOXPLOT OVERLAY PRODUCED HERE  #####
       }  ## finished adding boxes via boxplot() call
     }# FINISHED WITH OPTION 3 (default)
    } # end (post INTERMISSION)list-only input (factor=NULL) options

    if(verbose){print("PAG (@very end)"); print(PAG )}
    if(verbose){print("DOTS.i (@very end)"); print(DOTS.i )}

    if(zl){abline(v=0, lty=2)}  
    if(verbose){print(paste("recursion:",round(recursion,2)))}

    if(is.na(recursion) || (recursion == 1)){  # still getting through too many repeated (overtexting) times! (need another condition)
        mgp <- par('mgp')    

        axis.cex <- ifelse("axis.cex" %in% names(DOTS.i), yes=DOTS.i$axis.cex, no=par('cex'))
        lab.cex  <- ifelse("lab.cex"  %in% names(DOTS.i), yes=DOTS.i$lab.cex,  no=par('cex'))

        if(!('xaxt' %in% names(DOTS.i)) || DOTS.i$xaxt!='n'){axis(side=axis.label.cat.side, at=cats, labels=names(cats), cex=axis.cex)}
        if(!('yaxt' %in% names(DOTS.i)) || DOTS.i$yaxt!='n'){axis(side=axis.label.cnt.side,                              cex=axis.cex)}

        # MTEXT
        if(add==FALSE && is.na(recursion)){  # need a smoother check that doesn't over-do it too much with the text overprinting
         #dots.list <- DOTS.i[!(names(DOTS.i) %in% c('side','line','text')) &     # input not to trump 'xs', 'ys', recursion etc
         #                   !grepl(x=names(DOTS.i), pattern=.sf.OR.pattern(except='mtext'))]  # ... or any other sub.function params
         #mt.viauser <- dots.list[grepl(x=names(dots.list), pattern='^mtext.')]; 
  #names(mt.viauser) <- sub(pattern='mtext.', replacement='', x=names(mt.viauser))       # could also just pass via boxplot(pars=par())?? 
         if(!(PAG$xlab=='' && !('xlab' %in% names(DOTS.i))) ){     # these checks are overkill, will fix
          xlab <- ifelse('xlab' %in% names(DOTS.i), DOTS.i$xlab, PAG$xlab); if(xlab!=""){mtext(side=1, line=mgp[1], text=xlab, cex=lab.cex)}}
         if(!(PAG$ylab=='' && !('ylab' %in% names(DOTS.i))) ){                   # #...=mt.viauser)}}
          ylab <- ifelse('ylab' %in% names(DOTS.i), DOTS.i$ylab, PAG$ylab); if(ylab!=""){mtext(side=2, line=mgp[1], text=ylab, cex=lab.cex)}}
        }
    }    

    if(verbose) print('END plot.sparge()')
    invisible(pds) # RETURN used for mean-plotting
} # end function



lines.sparge <- function(x, f=NULL, pds, cat.order=1:2, horiz=TRUE, rb=nv(rainbow(length(pds)),names(pds)), pt.pch="+", pt.cex=1.5, ...){
    DOTS <- DOTS.i <- list(...)
    # x <- infert
    # f <- 'age ~ spontaneous | education'
    # pds <- nv(c(-0.3,0.0,0.3), c('0-5yrs','6-11yrs','12+ yrs'))
    if(!is.data.frame(x)) stop('currently x can only be a data.frame')
    if(is.null(f)) stop('"f" cannot be NULL (try a formula constructed as: "outcome ~ predictor.1 | predictor.2")')
    if(!.can.formula(f)) stop('currently, f can only be a formula')
    if(!(all(cat.order==1:2) || all(cat.order==2:1))) stop('cat.order must either equal 1:2 or 2:1')

    FPs <- .formula2parts(formula=f) 
    if(!all(sapply(x[,FPs$predictors], is.factor)))
        stop(paste('all model predictors:',paste(FPs$predictors, collapse='|'),'must be converted to factors'))
    if(!all(levels(FPs$predictors[cat.order[1]]) ==names(pds))) 
        stop(paste('the levels of the second (control) predictor:',FPs$predictors,'must match the names of "pds"'))
    if(length(FPs$predictors)==1) 
        stop('currently no support for single predictor models, try: "lines(sapply(x, median), 1:length(x)))" instead') #!!!!!!!!

    agg <- aggregate(x[,FPs$outcome], by=x[FPs$predictors], FUN=median)
    # at some point the above call to aggregate should  be replaced by groupBy
    # groupBy(df=infert, clmns='age', by=list('spontaneous','education'), aggregation='median')

    IN <- as.numeric(as.factor(agg[,FPs$predictors[1]])) + pds[agg[,as.character(FPs$predictors[2])]]
    OUT <- agg[,'x']  
    if(horiz) {Xs <- OUT; Ys <- IN;
         }else{Ys <- OUT; Xs <- IN;}

    CONTROL <- agg[, FPs$predictors[cat.order[2]]]
    ctrl.levs <- nv(1:length(levels(CONTROL)),levels(CONTROL))

    if('col' %in% names(DOTS.i)){color <- DOTS.i$col}else{color <- par('col')}
    if('pt.col' %in% names(DOTS.i)){pt.col <- DOTS.i$pt.col}else{pt.col <- par('col')}
    pt.cols <- rb  # default to coloring by default linen color
    if(cat.order[1]==2){pt.cols <- rb <- nv(rep(color,ctrl.levs[length(ctrl.levs)]),names(ctrl.levs))}
    for(sublev in names(ctrl.levs)){
        DOTS$col  <- rb[sublev]
        subset <- CONTROL==sublev
        do.call('lines', c(list(x=Xs[subset], y=Ys[subset]),  DOTS))
        points(x=Xs[subset], y=Ys[subset], col=pt.cols[sublev], pch=pt.pch, cex=pt.cex)
    }
}


