if(getRversion() >= '2.15.1') utils::globalVariables(c('CON')) 


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
	
		}
	)

	
# user level function calling rangeMapExport
rangeMap.export  <- function(con, dirName = dirName, ...) {
	
	x = new("rangeMap", CON = con)
	if(missing(dirName))  dirName =  dirname(dbGetInfo(con)$dbname)
	
	rangeMapExport(x, dirName = dirName, ...)	

}				



















