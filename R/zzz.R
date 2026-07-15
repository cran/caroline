
.citation <- paste('Schruth, D.M. (2026). "caroline: A Collection of Database, Data Structure, Data Conversion, Visualization, Reporting, and Utility Functions." Comprehensive R Archive Network, R package version', packageDescription("caroline")$Version ," https://CRAN.R-project.org/package=caroline")

.onAttach <- function(...){
    packageStartupMessage()
    packageStartupMessage("To display the package description and a full list of public functions, run: help(package='caroline')")
    packageStartupMessage("For an example of possible workflow usage, please explore the PDF vignettes in ./caroline/inst/doc/")
    packageStartupMessage()
    packageStartupMessage("To Cite this package please use:")
    packageStartupMessage()
    packageStartupMessage(.citation)
}
