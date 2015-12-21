.onAttach <- function(libname, pkgname) {
	dcf <- read.dcf(file=system.file("DESCRIPTION", package=pkgname) )
	packageStartupMessage("---------------------------------------------------------------------------------------")
	packageStartupMessage(paste('This is', pkgname, dcf[, "Version"] ))
	packageStartupMessage("---------------------------------------------------------------------------------------")
}



#' @import methods sp rgdal RSQLite rgeos raster  lattice ggplot2 RColorBrewer
NULL


#' @importFrom maptools getinfo.shape
NULL

#' @importFrom classInt classIntervals
NULL

#' @importFrom magrittr %>%
NULL

#' @importFrom foreach foreach %do% %dopar%
NULL

#' @importFrom data.table data.table
NULL

utils::globalVariables(c("i", "."))