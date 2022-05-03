\name{install.prev.pkg}
\alias{install.prev.pkg}
\title{Install the next oldest package}
\description{
This funciton will recursively search the package archives backwards in time until it finds a version that installs successfully. This function is useful for installing or troubleshooting package dependency trees where one or more packages "require" the very most recent version of R. Rather than upgrading your base R installation, you can use this function to search back in time until you find a version of the package that works with your existing version of R.
}
\usage{
install.prev.pkg(pkg.nm, version=NULL, 
  repo.url='https://cran.r-project.org/src/contrib/Archive/')
}
\arguments{
\item{pkg.nm}{The package name.}
\item{version}{The version number as #.#-# or #.#.# .}
\item{repo.url}{The base url for archives of old versions of packages on CRAN.}
}
\value{
NULL
}
\examples{
#install.prev.pkg('mnormt')
#install.prev.pkg('mvtnorm')
}
\keyword{manip}

