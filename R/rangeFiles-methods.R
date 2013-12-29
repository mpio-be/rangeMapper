

setGeneric("rangeFiles", function(object, ...)   					standardGeneric("rangeFiles") )


setMethod("rangeFiles",  
		signature = "rangeFiles", 
		definition = function(object, ...){
		dir.list = list.files(object@dir, recursive = TRUE, full.names = TRUE, pattern = ".shp$")
	
	if(object@polygons.only){
		nfo = lapply(dir.list, getinfo.shape)
		is.poly =  unlist(lapply(nfo, function(x) x$type == 5))
		dir.list = dir.list[is.poly]
		}
	
	if(object@ogr) 
		res = data.frame(dsn = dirname(dir.list), layer = gsub(".shp", "", basename(dir.list)), stringsAsFactors = FALSE) else
		res = dir.list
		res
	} 
)


#user level function
selectShpFiles <- function(dir,  ...) {
	rangeFiles(new("rangeFiles", dir =dir, ...))
	}


