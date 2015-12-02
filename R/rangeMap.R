
#' Class "rangeMap" to formally describe a rangeMapper sqlite file.
#'
#' This is the basic class of the package and the only one documented and exported.
#'
#'
#' @name rangeMap-class
#' @aliases rangeMap-class summary,rangeMap-method, rangeMap
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("rangeMap", \dots{})}.
#' @author Mihai Valcu, \email{valcu@@orn.mpg.de}
#' @seealso \code{\link{rangeMapper}}, \code{\link{processRanges}}
#' @export
#' @keywords classes
#'
rangeMap <- function(path){
	new("rangeMap", CON = dbConnect(RSQLite::SQLite(), dbname= path) )
	}
