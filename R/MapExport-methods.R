utils::globalVariables(c('CON'))

setGeneric("rangeMapExport", function(object,dirName, ...)  	    standardGeneric("rangeMapExport") )

# method for  exporting to external files
setMethod("rangeMapExport",
	signature  = c(object = "rangeMap", dirName = "character"),
		definition = function(object, dirName, ...) {

		mapnams = dbGetQuery(object@CON,
					'select name from sqlite_master where type = "table" and
						(tbl_name like "MAP_%")')$name

		if(length(mapnams) == 0) stop("The project does not contain any MAP tables!")

		wd = setwd(dirName)

		for(i in 1:length(mapnams)) {

			mapi = rangeMap.fetch(object@CON, mapnams[i])

			writeGDAL(mapi , paste(getwd(), paste(mapnams[i], "tiff", sep = "."), sep = .Platform$file.sep), ... )
		}

		setwd(wd)

		})

#' Export \sQuote{MAP} tables
#'
#' Export \sQuote{MAP} tables as single-band geotiff files
#'
#'
#' @aliases rangeMap.export
#' @param con An sqlite connection pointing to a valid \code{rangeMapper}
#' project.
#' @param dirName The directory name where the \sQuote{MAP}s will be exported.
#' If missing, the \sQuote{MAP}s will be exported in project's directory
#' @param \dots Further arguments to pass to \code{\link{writeGDAL}}
#' @author Mihai Valcu \email{valcu@@orn.mpg.de}
#' @references Valcu, M., Dale, J. and Kempenaers, B. (2012) rangeMapper: A
#' platform for the study of macroecology of life history traits. 21(9). (DOI:
#' 10.1111/j.1466-8238.2011.00739.x)
#' @export
#' @keywords export
#'
rangeMap.export  <- function(con, dirName = dirName, ...) {
	x = new("rangeMap", CON = con)
	if(missing(dirName))  dirName =  dirname(dbGetInfo(con)$dbname)

	rangeMapExport(x, dirName = dirName, ...)
  	}



















