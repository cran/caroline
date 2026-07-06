dbWriteTable2 <- function(con, table.name, df, fill.null = TRUE, add.id=TRUE, row.names=FALSE, pg.update.seq=FALSE, ...){
  requireNamespace("DBI")
  fields <- DBI::dbListFields(con, table.name)
  fields <- fields[!grepl('\\.\\.pg\\.dropped',fields)]
  
  ## add id column if missing
  if(add.id){
    last.id.list <- DBI::dbGetQuery(con, paste("SELECT id FROM", table.name,"ORDER BY id DESC LIMIT 1"))
    if(length(last.id.list)==0)
      n <- 0
    else
      n <- last.id.list[[1]]
    df$id <- 1:nrow(df) + n
  }
  ## look for unloadable columns in the df
  names(df) <- tolower(names(df))
  names(df) <- gsub("\\.",'_',names(df))
  
  clmn.match <- match(names(df), fields)
  if(any(is.na(clmn.match)))
    warning(paste("Found '",names(df)[is.na(clmn.match)], "' not in fields of '", table.name,"' table. Omiting.\n", sep=''))
  
  ## add missing fields to df
  field.match <- match(fields, names(df))
  if(sum(is.na(field.match))>0 & fill.null == TRUE){
    message("creating NAs/NULLs for for fields of table that are missing in your df")
    nl <- as.list(rep(NA, sum(is.na(field.match))))
    df.nms.orgnl <- names(df)
    df <- cbind(df, nl)
    names(df) <- c(df.nms.orgnl, fields[is.na(field.match)])
  } 	
  
  ## reorder df columns as per field order
  reordered.names <- names(df)[match(fields, names(df))]
  if(any(is.na(reordered.names)))
    stop('Too many unmatched columns to database column list. Stopping')
  df <- df[ ,reordered.names]

  
  ## BEGIN ERROR CHECKING
  r <- DBI::dbSendQuery(con, paste("SELECT * FROM", table.name,"ORDER BY id DESC LIMIT 1"))
  db.col.info <- DBI::dbColumnInfo(r); rownames(db.col.info) <- db.col.info$name

  ## check for na's which might prevent a load
  null.OK <- nv(db.col.info,'nullOK')
  reqd.fields <- names(null.OK[!null.OK])
  na.cols <- sapply(df, function(x) any(is.na(x)) )
  req.miss <- na.cols[reqd.fields]
  if(any(req.miss))
    stop(paste("Didn't load df because required field(s)", paste(names(req.miss)[req.miss],collapse=', '),"contained missing values"))
  
  ## check for length mismatches    
  db.precisions <- nv(db.col.info, 'precision')
  df.nchars <- sapply(df, function(c) max(nchar(c)))
  prec.reqd <- db.precisions > 0
  too.long <- db.precisions[prec.reqd] < df.nchars[prec.reqd]
  if(any(too.long))
    stop(paste("Didn't load df because fields", paste(names(df.nchars)[prec.reqd][too.long],collapse=', '),'were too long'))

  ## check for type mismatches
  db.sclasses <- nv(db.col.info,'Sclass')
  df.classes <- sapply(df, class)
  type.mismatches <- names(df.classes)[db.sclasses != df.classes & !na.cols]
  #if(length(type.mismatches)>0)
  #  warning(paste('The dataframe columns:',paste(type.mismatches, collapse=','),'may have type mismatches from their sclass mappings to the database table fields.'))
    
  DBI::dbClearResult(r)
  
  ## check unique constrains
  #r <- dbGetQuery(con, paste("SELECT constraint_name FROM information_schema.table_constraints WHERE table_name = '",table.name,"'", sep=''))
  #if(nrow(df) != length(unique(apply(df[,c('day','file')],1, paste, collapse='.')))
  #stop
  
  ## load table
  print(paste("loading", table.name, "table to database"))
  db.write <- DBI::dbWriteTable(con, table.name, df, row.names=row.names, ...)
  
  #updating postgresql sequence
  if(pg.update.seq){
    if(identical(class(con),'PostgreSQLConnection')){
      r <- DBI::dbSendQuery(con, paste("SELECT pg_catalog.setval(pg_get_serial_sequence('",table.name,"', 'id'), (SELECT MAX(id) FROM ",table.name,")+1);",sep=''))
      DBI::dbClearResult(r)
    }else{
      stop('pg.update.seq=TRUE flag not compatable with database connection type')
    }
  }
  
  if(db.write & add.id)
    invisible(df$id)
  else
    return(db.write)
}



nerge <- function(l, method=c('rownames','lookup'), ...){ ## named data.frame or vector merge   #keep.row.clmn=FALSE, 
  if(length(method)<3){
    if(!all(method %in% c('rownames','lookup'))) stop("method options are 'rownames' or 'lookup'")
    if(length(method)==2){ warning("method is either 'rownames' or 'lookup', setting to 'rownames' default")
    method <- 'rownames'}
  }
  #if(any(sapply(names(l), function(ln) 'lookup' %in% ln))) stop('lookup is a special reserved word used in nerging')
  if(!all(sapply(l, function(k) is.data.frame(k) | is.vector(k) | is.factor(k))))
     stop('list elements must be either of class data.frame or of type vector (or factor)')
  if(length(l) < 2)
    stop('list l must have at least 2 elements')
  if(is.null(names(l))){
    warning("each merge element in the list 'l' should have a name. making some up.")
    names(l) <- letters[1:length(l)]
  }
  if(method=='rownames'){
  if(!all(sapply(l, function(j) !is.null(rownames(j)) | !is.null(names(j)))))
    stop('all list elements must have named components (dataframes must have row names)')
  }
  if(all(sapply(l, is.data.frame)) && all(sapply(l, dim)[2,]==1)){
    warning("all dataframes in 'l' have only one column, consider converting them to named vectors so that l's list names can be used for the merged df column names (eg, via l <- lapply(l, nv, 1))")
  }

  lookup.ids <- NULL
  lookup.names <- m(names(l[[1]]), pattern='^([^\\.]+).id$')
  if(method!='lookup'){  
    if(any(!is.null(lookup.names)) & any(lookup.names %in% names(l))) 
      warning('found a "<table>.id" in the column names of the first entity of "l", should we set method="lookup"?')
  }else{# auto id lookup
    if(!is.data.frame(l[[1]])) 
      stop("1st element should be a DF w/ '<entity>.id' columns that simply (star DB topology) reference the other list entities with corresponding lookup names)")
    #lookup.names <- m(names(l[[1]]), pattern='^([^\\.]+).id$')
    lookup.names <- lookup.names[!is.na(lookup.names)]  #this really should be handled inside "match"
    if(sum(!is.na(lookup.names) != length(l)-1)) 
      stop("the number of lookup ids in the first list entity (a dataframe) should equal the length of the entity list minus one")
    if(!all(names(l)[-1] %in% lookup.names))
      warning(paste(c("there are names (", paste(names(l), collapse=','), ") that don't match the '.id' columns in df1 (",paste(lookup.names, collapse=','),")"), collapse=''))
    lookup.names <- lookup.names[match(names(l), lookup.names, nomatch=F)] # re-odering   # IF NA's are removed -1 must be added to 'e' index counting below
    lookup.ids <- nv(paste(lookup.names, '.id',sep=''), lookup.names)
  }
  df <- .vle2df(l, names(l)[1], mthd=method)
  for(e in 2:length(l)){
    if(method=='rownames'){
      df <- merge(x=df, y=.vle2df(l, names(l)[e], mthd=method), by='rownames', ...) 
      rownames(df) <- df$rownames
    }else{
      message(paste(c("Joining main dataframe onto '",names(l)[e] ,"' via '",lookup.ids[names(l)[e]],"'"), collapse=""))
      df <- merge(x=df, y=.vle2df(l, names(l)[e], mthd=method), 
                       by.x=lookup.ids[names(l)[e]], 
                       by.y='lookup', ...)
    }
  }
  df[,method] <-  NULL  
  ## removing appended colnames if unnecessary
  orig.names <- sub(paste('\\.(',paste(names(l),collapse='|'),')$',sep=''),'', names(df))
  if(length(orig.names)== length(unique(orig.names)))
    names(df) <- orig.names
  df[, grep(names(df),pattern='(^lookup\\.|\\.id$)')] <- NULL
  return(df)
}


.vle2df <- function(vl, vei, mthd='rownames'){ # helper function for nerge above
  ## vector list element to dataframe (preserves list element names)

  if(is.data.frame(vl[[vei]])){
    df <- vl[[vei]]
    if(mthd!='lookup'){
       names(df) <- paste(names(df),'.',vei,sep='')}   
  }else{
    df <- as.data.frame(vl[[vei]])
    colnames(df) <- vei
    rownames(df) <- names(vl[[vei]])
  }
  df[,mthd] <- rownames(df) 
  df
}




