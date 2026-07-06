# caroline (next version)

* sparge (plot): plans to remedy xlab & ylab redundantly-superimposed margin printing
* sparge (lines): improve line plotting logic for simpler models (so that just the medians of the first predictor are used in point plotting/joining)   
* groupBy & bestBy: implement support for providing a list to the "by" argument so that it returns a tabular or nested lists of results
* nerge: add a way to recycle the names of the merged elments so that one of them is kept as a clmn via a, say, 'keep.row.clmn' param
 
# caroline 1.1.1

* Overhauled plot.sparge() 
*  - added *lines.sparge()* a line-plotting accessory function x1 by x2 or x2 by x1 sub-groupings (for f=y ~ x1 | x2)  
*  - implemented ability to *relay most arguments* of the form boxplot.<boxplot.param> and legend.<legend.param> *to corresponding sub-routines*
*  - to more comprehensively and systematically *handle* all sorts of *user input* in a friendly way,
*  - to better accommodate *ambiguous input* for both *f* and *cat.names*
*  - better all around *documentation* in man page and *commenting* of code
* Also implemented some features/fixes/documentation with/for *heatmatrix, nerge, pct, nv, bestBy, & tab2dv*
* Eg: heatmatrix() has some friendlier defaults (xlab/ylab) with an added option to specify the 'text.colors' superimposed over the matrix cells
* Eg: nerge() now has an entirely new (more truly *database-style*) way of *merging* using *".id" column-name suffixes* ()for simple star schema) 
* Eg: nerge() ... also ... fixed a bug where more than two list elements (eg, named vectors) couldn't be joined together
* Eg: pct() beyond just dataframes, it now accepts vector imput, plus rounding & appending a "%"  
* Eg: nv() documentation [& operations]  improved with better in-function messaging
* Eg: tab2dv() colnames may get garbled by underlying dataframe() unless checknames=F is passed (see details in help(tab2dv)
* Eg: bestBy() can returns the "top=n" best for each grouping for both SQL and non-SQL versions
* added several convenience functions for input checking: .can.factor() .can.formula() .is.formula()

# caroline 1.0.0

* Fixed a bug with plot.sparge and the point-swath width overlap (Though, there does remain  a small width problem with plot-area under-utilization)

# caroline 0.9.8

* plot.sparge() added!

# caroline 0.9.0

* "distro.dots" implemented -> later to be renamed to "sparge plot"
* violins() removed due to disinterest in maintaining its overly-complex (eg codebase/package) dependencies ('sm')

# caroline 0.8.0

* Added new "install.prev.pkg()" function to help iteratively load slightly older CRAN pacakges until dependencies work again
* last version with violins() function

# caroline 0.7.6

* first independently maintained version uploaded to CRAN
