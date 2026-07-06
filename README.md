# caroline: A Collection of Database, Data Structure, Visualization, and Utility Functions for R.
#  (this Rmarkdown file is currently unused and under development and exists for example purposes only)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/caroline)](https://CRAN.R-project.org/package=caroline)
[![CRAN_Downloads_Badge](https://cranlogs.r-pkg.org/badges/last-month/caroline)
## Citation

If you'd like to cite 'caroline', please cite as follows:

"David M. Schruth (2026) "caroline A Collection of Database, Data Structure, Visualization, and
        Utility Functions for R.",
        "Comprehensive R Archive Network",
        "https://CRAN.R-project.org/package=caroline")


## Installation

Install the stable release from CRAN:

```r
install.packages("caroline")
```

Or install directly from github using devtools

```r
library(caroline)
```

## Usage

See the [online package vignette](./inst/doc/Schruth-caroline-GlobalForestLosses.pdf) for more examples:

```r
browseVignettes("caroline")
```

Take a look at the built-in data:

```r
head(treeloss)
```

Basic sparge plot using built-in data:

```r
plot.sparge(treeloss)
```

Get help:

```r
?n
?nv
?pct
?nerge
?tab2df
?sstable
?groupBy
?bestBy
?sparge
?heatmatrix
?confound.grid
```

## Notes

* This package has been purposefully simplified for the sake of maintainability and to reduce dependence on other more distantly complex uni-'verses' of code.
