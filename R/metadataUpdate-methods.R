

setGeneric("metadataUpdate", function(object, FUN,name, map, parallel, ...)  standardGeneric("metadataUpdate") )

 # Method 1: using  a SpatialGridDataFrame object
setMethod("metadataUpdate",  
		signature = c(object = 'rangeMap', FUN = 'function', name = 'character', 
					 map = 'SpatialGridDataFrame', parallel = 'missing'), 
		definition = function(object, FUN, name, map, ...){ # ... goes to FUN
			Startprocess = Sys.time()
			
			# only use 1st band of the raster
			if(length(names(map)) > 1) {
				warning( paste("The SpatialGridDataFrame has more than one band, only", dQuote(names(map)[1]), "will be used!") )
			map = map[names(map)[1]]
			}
			
			# check if metadata_ranges is populated
			empty = dbGetQuery(object@CON, "SELECT count(bioid) from metadata_ranges") == 0
			if(empty)
				dbGetQuery(object@CON, "INSERT INTO metadata_ranges SELECT distinct bioid from ranges") 
			
			# add new column
			if( is.element(name, c(names(dbGetQuery(object@CON, 'SELECT * FROM metadata_ranges limit 1')), SQLKeywords(object@CON) ) ) )
				stop(paste(dQuote(name), "allready exists or is not a valid SQL name!") )
			name = RSQLite::make.db.names(object@CON, name)
			dbGetQuery(object@CON, paste("ALTER TABLE metadata_ranges ADD COLUMN", name ,"NUMERIC;") )
			
			# get ranges listing
			mr = dbGetQuery(object@CON, "SELECT bioid from metadata_ranges")$bioid
						
			# loop through ranges,  apply FUN and update metadata_ranges
			
			message("Updating metadata_ranges ...")
			pb = txtProgressBar(min = 0, max = length(mr), char = ".", style = 3)

			for( i in 1:length(mr) ) {
				idi = mr[i]
				ri = rangeFetch(object, idi)
				sraster = map[!is.na(over(map, ri)), ] #  SpatialGridDataFrame subset
				x = as.numeric(sraster@data[,1])
				res = FUN(x, ...) #apply FUN
				
				if( any(length(res) > 1 | res%in%c(-Inf, Inf)) ) {
					.dbRemoveField(object@CON, 'metadata_ranges', name)
					stop( paste("FUN returned ", dQuote(res), ". It should only return a non-infinite numeric vector of length 1.", sep = '') )
				}
				
				if(!is.na(res)) 
					dbGetQuery(object@CON, paste('UPDATE metadata_ranges SET' ,name, '=' ,res, 'WHERE bioid =' ,shQuote(idi) ))
				setTxtProgressBar(pb, i)
			}
			
	# last Msg
	message( paste("Done in ", round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins") )
	} 
)


# user level function calling metadataUpdate
metadata.update  <- function(rangeMap, FUN, name, map, overwrite = FALSE, ...) {
	if(overwrite) 
	 .dbRemoveField(rangeMap@CON, 'metadata_ranges', name)
	metadataUpdate(object = rangeMap, name = name, FUN = FUN, map = map, ...)
}				

	
	
	
	
	
	
	
 