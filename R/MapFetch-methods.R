

# 2013-Nov-21 10:09:39


setGeneric("rangeMapFetch", function(object, ...) 					standardGeneric("rangeMapFetch") )

setMethod("rangeMapFetch",  
	signature  = "rangeMapFetch", 
		definition = function(object) {
		
    	#build tableName(s)
		mapNam = paste(object@MAP, object@tableName, sep = "")

		# map variable
		mapvar = sapply(mapNam, function(x)
					setdiff(dbGetQuery(object@CON, paste("pragma table_info(", x, ")"))$name, object@ID ) )		
		# sql string
		dotid = paste('x', 1:length(mapNam), sep = "")
		mapdat = paste(paste(paste(dotid,mapvar, sep = "."), object@tableName, sep = " as "), collapse = ",")
		
		sql = paste("SELECT c.x, c.y,", mapdat, 
		"from canvas as c LEFT JOIN",paste(paste(mapNam, dotid, "on c.id = ", dotid, ".id"), collapse = " LEFT JOIN "))

		
		map = dbGetQuery(object@CON, sql)
	
		coordinates(map) = ~ x + y

		p4s = dbReadTable(object@CON, object@PROJ4STRING)[1,1]
		
		proj4string(map) = CRS(p4s)
		
		gridded(map) = TRUE
		
		map = new("SpatialPixelsRangeMap", map, mapvar    = mapvar)
		return(map)
		}
	)	
	
# user level functions 
rangeMap.fetch <- function(con, maps) { 
	
	if(missing(maps)) maps = dbGetQuery(con, 'select name from sqlite_master where type = "table" and tbl_name like "MAP_%"')$name

	maps = gsub("MAP_", "", maps)
	
	x = new("rangeMapFetch", CON = con, tableName = maps)
	rangeMapFetch(x)	

}

rangeFetch <- function(rangeMap, bioid) {
				
		if( nrow(dbGetQuery(rangeMap@CON, paste("SELECT * from canvas limit 1"))) == 0)
			stop('Empty project!')
		
		p4s = CRS(dbReadTable(rangeMap@CON, rangeMap@PROJ4STRING)[1,1]) 	# proj4string
		cs2 = dbGetQuery(rangeMap@CON, "select * from gridsize")[1,1]/2 		# 1/2 grid size
		
		d = dbGetQuery(rangeMap@CON, paste("SELECT c.id, c.x, c.y from canvas c join ranges r on c.id = r.id where r.bioid = ", shQuote(bioid) ) )
		if(nrow(d) == 0)	
			stop(paste(dQuote(bioid), 'is not a valid name!'))		
		
		d = split(d, d$id)
		
		d = lapply(d, function(z) {
			xi = z$x
			yi = z$y
			x = c(xi-cs2, xi-cs2, xi+cs2, xi+cs2, xi-cs2)
			y = c(yi-cs2, yi+cs2, yi+cs2, yi-cs2, yi-cs2)
			Polygons(list(Polygon(coords=cbind(x, y) )), ID = z$id)
			})
		
		res = SpatialPolygons(d, proj4string= p4s)

		res = gUnionCascaded(res)

		res	
}





















