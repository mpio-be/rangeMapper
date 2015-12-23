.onAttach <- function(libname, pkgname) {
	dcf <- read.dcf(file=system.file("DESCRIPTION", package=pkgname) )
	packageStartupMessage(paste('This is', pkgname, dcf[, "Version"] ))
    }



#' @import sp methods RSQLite lattice ggplot2
NULL

#' @importFrom magrittr %>%
NULL

#' @importFrom foreach foreach %do% %dopar% getDoParRegistered getDoParWorkers
NULL

#' @importFrom data.table data.table setDT setattr melt
NULL

#' @importFrom rgeos gUnionCascaded readWKT
NULL

#' @importFrom rgdal CRSargs OGRSpatialRef readGDAL readOGR  writeGDAL
NULL

#' @importFrom raster raster rasterToPolygons projection
NULL

#' @importFrom maptools getinfo.shape
NULL

#' @importFrom classInt classIntervals
NULL

#' @importFrom RColorBrewer brewer.pal.info brewer.pal
NULL

# data.table and foreach 'values'
utils::globalVariables(c("i", ".", "value"))