\name{nerge}
\alias{nerge}
\title{Named Merge}
\description{
This function is a wrapper for merge that supports merging multiple vectors and or dataframes
}
\usage{
nerge(l, method=c("rownames", "lookup"), ...)
}
\arguments{
\item{l}{A named list of named vectors (and/or dataframes)}
\item{method}{A char string of either 'rownames' or 'looup' specifying if unique rownames or db-style '.id' look-ups should join the elements)}
\item{...}{Other parameters passed on to each sub-merge}
}
\seealso{
\link{merge}

}
\value{
A data frame composed of vectors and dataframes with corresponding rownames (& DB-style lookup ".id" columns) 
}
\examples{

## named vectors and dataframes ('rownames' method)
df <- data.frame(a=c(6,7,8), b=c(9,8,7))
rownames(df) <- c('a','d','c'); print(df)

vector.list <- list(p=nv(c(1.23, 1.423, 2.343), c('z','y','x')), 
                    q=nv(c(6.34,7.34, 12.545),c('z','w','y')))
df.2 <- nerge(vector.list, method='rownames', all=TRUE); print(df.2)

mixed.list <-  list(df=df, z=nv(c(4,1),c('c','d')))
df.3 <- nerge(mixed.list,      method='rownames', all=TRUE); print(df.3)
df.3 <- nerge(mixed.list[2:1], method='rownames', all=TRUE); print(df.3)


vect.x3.list <- c(list(p=vector.list[['p']], q=vector.list[['q']], 
                       r=nv(c(32, 5, 43.2), c('x','y','w'))))
df.4 <- nerge(vect.x3.list, all=TRUE); print(df.4)

# three single column dataframes with unique names
df.P <- data.frame(X=nv(1:3,letters[1:3]))
df.Q <- data.frame(Y=nv(3:5,letters[3:5]))
df.R <- data.frame(Z=nv(5:7,letters[5:7]))
nerge(list(P=df.P,Q=df.Q,R=df.R), method='rownames',all=TRUE)

# ... variation: auto append of list element names for duplicates
df.R <- data.frame(Y=nv(5:7,letters[5:7]))
nerge(list(P=df.P,Q=df.Q,R=df.R), method='rownames',all=TRUE)


## database style lookup table merges ('lookup' ['.id'] method)
lets.v <- nv(sample(letters[1:26],26),1:26)
nums.v <- nv(sample(1:10,10),1:10)
df.db <- data.frame(cols=sample(colors(),5), 
                  lets.id=sample(1:26,5), nums.id=sample(1:10,5))
join.these <- list(cols=df.db, lets=lets.v, nums=nums.v)
nerge(l=join.these, method='lookup')


}
\keyword{manip}

