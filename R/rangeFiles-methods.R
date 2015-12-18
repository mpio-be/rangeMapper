setGeneric("rangeFiles", function(object, ...)   					standardGeneric("rangeFiles") )

setMethod("rangeFiles",
	signature = "rangeFiles",
	definition = function(object, ...){
	dir.list = list.files(object@dir, recursive = TRUE, full.names = TRUE, pattern = ".shp$")

	if(object@polygons.only){
	nfo = lapply(dir.list, maptools::getinfo.shape)
		is.poly =  unlist(lapply(nfo, function(x) x$type == 5))
		dir.list = dir.list[is.poly]
		}

	if(object@ogr)
		res = data.frame(dsn = dirname(dir.list), layer = gsub(".shp", "", basename(dir.list)), stringsAsFactors = FALSE) else
		res = dir.list
		res
		} )


#' Select (recursively) shape files
#'
#' Returns the file path to all \sQuote{.shp} polygons in a directory.
#'
#'
#' @aliases selectShpFiles rangeFiles rangeFiles-methods
#' rangeFiles,rangeFiles-method
#' @param dir character string specifying the directory containing .shp files.
#' @param \dots currently ignored
#' @return Either a \code{\link{data.frame}} or a character vector is returned.
#' @note The function uses \code{\link[maptools]{maptools::getinfo.shape}} to only select
#' polygon files (aka type 5).
#' @author Mihai Valcu \email{valcu@@orn.mpg.de}
#' @seealso \code{\link{rangeMap.save}}.  \code{\link[maptools]{maptools::getinfo.shape}}
#' @references Valcu, M., Dale, J. and Kempenaers, B. (2012) rangeMapper: A
#' platform for the study of macroecology of life history traits. 21(9). (DOI:
#' 10.1111/j.1466-8238.2011.00739.x)
#' @keywords misc
#' @examples
#'
#' f= system.file(package="rangeMapper", "extdata", "wrens", "vector")
#' res = selectShpFiles(f, ogr = TRUE, polygons.only = TRUE)
#' head(res)
#'
#'
#' @export selectShpFiles
selectShpFiles <- function(dir,  ...) {
	rangeFiles(new("rangeFiles", dir =dir, ...))
	}


