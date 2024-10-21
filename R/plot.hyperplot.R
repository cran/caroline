
  
hyperplot <- function(x, y=NULL, annout=1:length(x), name='hyperplot.imagemap', w=72*8, h=72*6, link='internal', browse=TRUE, cex=1, ...){
 
  ## generate output paths
  img.path <- paste(name,'.png',sep='')
  html.path <- paste(name,'.html',sep='')

  ## create image
  png(img.path, width=w, height=h)

  if(is.data.frame(x)) #class(x)=='data.frame')
    stop('for data.frame input: x and y vectors should be in the annout table with x & y used to specify column names')
    	
  ## plot
  if(is.null(y)){  
    # x as an object with a native 'plot' method and xyc return data.frame
    xyc <- plot(x, ...)
    if(ncol(xyc)<2 | ncol(xyc)>3)
      stop('plot(x) must return a rownamed dataframe or matrix of 2 or 3 columns: x,y and optionally cex (in that order)')
    x <- xyc[,1]
    y <- xyc[,2]
    if(ncol(xyc)==3)
      cex <- xyc[,3]
    else
      cex <- 1
    idx <- rownames(xyc)
  }else{          

    if(all(is.character(c(x,y))) & all(sapply(list(x,y), length)==1)){ 
      # x and y as column names
      if(all(c(x,y)%in% names(annout)))
        idx <- rownames(annout)
      else
        stop('x and y must exist in annout')
      
      x <- annout[,x]
      y <- annout[,y]
      idx <- rownames(annout)
    }else{    
      # x and y as named numeric vectors and annout as index to x (and y)

      if(is.data.frame(annout) | is.matrix(annout)){
        annout.idx  <- row.names(annout)
      }else{ # is.vector
        annout.idx <- annout  
        link='none'
      }
      
      if(is.null(names(x))){ # annout as a character vector or named dataframe
        if(is.numeric(annout.idx)){
          idx <- as.character(1:length(x))  # annout index is converted below in df check
        }else{
          if(length(annout.idx) == length(x))
            idx <- annout.idx 
          else
            stop("length of name index supplied form annout must be same as length as x")
        }    
      }else{  # annout as index into names of x
        idx <- names(x)
        if(is.numeric(annout.idx))
          annout <- names(x)[annout.idx]
      }
    }
    
    if(!is.numeric(x) | !is.numeric(y) | length(x) != length(y))
      stop('x and y must be numeric vectors of equal length')
    
    plot(x, y, ...)

  }
   
  ## build x, y coordinates, names & point sizes 
  map <- data.frame(idx=idx, x=x, y=y, cex=cex, row.names=idx)
  
  ## grab figure & plot dimentions
  mai <- par('mai')*72 #margins
  pin <- par('pin')*72 #plot dim
  usr <- par('usr')
  
  usr.xd <-diff(usr[1:2])
  usr.yd <-diff(usr[3:4])

  pin.xd <-pin[1]
  pin.yd <-pin[2]

  ## save image
  dev.off()
  
  ## determine outlier points to annotate
  if(is.data.frame(annout))
    annout$nm <- rownames(annout)
  else
    annout <- data.frame(nm=as.character(annout))

  if(length(link) != 1 | !is.character(link))
    stop("'link' must be a character string specifying if points are linked 'internal'ly \
           or which column of annot to use as external hyperlinks")

  if(!link %in% c('none','internal',colnames(annout)))
    stop("'link' must be either 'none', 'internal' or a column name of annout")
  
  if('out' %in% names(annout))
    annout <- annout[annout$out, ]

  ## subset coords by only the desired outlier list  
  map <- subset(map, idx %in% annout$nm)
  
  if(nrow(map) < 1)
    stop('no points to map because none of the annotations matched?')
  
  if(link == 'none')
    map$href <- ''
  else    
    if(link == 'internal')
      map$href <- paste('#',sub(' ','_',map$idx), sep='')
    else
      map <- nerge(list(map=map, href=nv(as.character(annout[,link]), annout$nm)))

  for(clmn in names(annout)[sapply(annout, is.character)])
    if(substr(annout[1,clmn],1,4) %in% c('http','www.'))
      annout[,clmn] <- paste("<a target='_blank' href='",annout[,clmn],"'>",annout[,clmn],"</a>",sep='')
  
  ## translate user coordinate space into image output coordinates
  map$x <-  (map$x - usr[1])/usr.xd * pin.xd + mai[2]
  map$y <- (-map$y + usr[4])/usr.yd * pin.yd + mai[3]
  map$r <- map$cex * 3

  ## write HTML map
  sink(html.path)

  cat('<html>\n')
  cat(paste('<img src="',img.path,'" width="',w,'" height="',h,'" usemap="#',name,'"/>', sep=''),'\n')
  cat(paste('<map name="',name,'">',sep=''))    
  with(map, cat(paste('<area shape="circle" coords="',x,',',y,',',r,'" href="',href,'" title="',idx,'"/>\n',sep='')),'\n')
  cat('</map>')

  if(link == 'internal' & is.data.frame(annout)){ #class(annout)=='data.frame'){
    cat('<br><h5>Annotations</h5>')
    cat('<table border=1>','\n')
    cat(paste('<tr><th></th>', paste('<th>', names(annout),'</th>',collapse=''), '</tr>\n'))
    for(i in 1:nrow(annout))
      cat(paste('<tr><td><a name="',sub(' ','_',annout$nm[i]),'"></a></td>',
                paste('<td>',annout[i,],'</td>',collapse=''), '</tr>\n',sep=''))
    cat('</table>','\n')
  }
  cat('</html>')
  cat(rep('<br>',60)) ## so hyperlink jumps don't run into the bottom of the browser window')
  sink()

  ## open browser 
  if(browse)
    browseURL(html.path)
}


