

if (!isGeneric("summary")) { 
	setGeneric("summary", function(object, ...) standardGeneric("summary")) 
	}

summary.rangeMap <- function(object, ...) {
    out = list()
	out[["class"]] = class(object)

	dbinfo = dbGetInfo(object@CON)
	out[["Project_location"]] = dbinfo$dbname
	out[["SQLite_version"]] = dbinfo$serverVersion

	if( nrow(dbGetQuery(object@CON, paste("select", object@ID, "from", object@CANVAS, "limit 1") )) == 0)
	out[["empty_project"]] = "Empty rangeMapper project." else {
	
		out[["Proj4"]]    = dbReadTable(object@CON, object@PROJ4STRING)[1,1]
		out[["CellSize"]] = dbReadTable(object@CON, object@GRIDSIZE)[1,1]
		out[["Extent"]]   = dbReadTable(object@CON, object@BBOX)
		
		tbs = dbGetQuery(object@CON, "select name from sqlite_master where type = 'table' ")$name
		
		out[["BIO_tables"]] = paste(  gsub(object@BIO, "", tbs[grep(object@BIO, tbs)]), collapse = ";" )
		out[["MAP_tables"]] = paste(  gsub(object@MAP, "", tbs[grep(object@MAP, tbs)]), collapse = ";" )
	 
		mtd =.is.empty(object@CON, object@METADATA_RANGES)
		out[[object@METADATA_RANGES]]= paste(object@METADATA_RANGES, "is empty:", mtd, collapse = ";" )

	}
	
	class(out) = "summary.rangeMap"
	out
}

print.summary.rangeMap <- function(x, ...) {


	message(paste(paste(names(x), ":", x), collapse = ";"), ...)
	

}

setMethod("summary", "rangeMap", summary.rangeMap)
















