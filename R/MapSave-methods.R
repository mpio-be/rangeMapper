
# accessor functions for methods
.subsetSQLstring   <- function(dbcon, subset = list() ) {

	if(length(subset) == 0) sql = NULL else {
	if(is.null(names(subset))) stop(sQuote('subset'), " must be a named list!")

	m = subset[grep("^MAP_", names(subset))]
	b = subset[grep("^BIO_", names(subset))]
	r = subset[which(names(subset)=="metadata_ranges")]

	msql = if(length(m) > 0) paste(paste("r.id in (SELECT id FROM", names(m), "WHERE", m, ")"), collapse = " AND ") else NULL
	bsql = if(length(b) > 0) paste(paste("r.bioid in (SELECT", 
			sapply(names(b), function(x) .extract.indexed(dbcon, x)) ,
				"FROM", names(b), "WHERE", b, ")"), collapse = " AND ") else NULL
	rsql = if(length(r) > 0) paste(paste("r.bioid in (SELECT bioid FROM", names(r), "WHERE", r, ")"), collapse = " AND ") else NULL

	sql = paste( c(msql, bsql, rsql), collapse = " AND ")
	}
	sql	
}


setGeneric("rangeMapSave", function(object,FUN,formula, ...)  standardGeneric("rangeMapSave") )

# method for species richness,
setMethod("rangeMapSave",  
		signature = c(object = "rangeMapSave", FUN = "missing", formula = "missing"), 
		definition = function(object, FUN, formula, ...){
			
			if(length(object@tableName) == 0) object@tableName = "species_richness"
			
			#build tableName
			tableName = paste(object@MAP, object@tableName, sep = "")
			
			# build sql subset
			sset = .subsetSQLstring(object@CON, object@subset)
			
			# build sql string
			richnessSQL = paste("SELECT id, count(r.id) as", object@tableName ,"from ranges as r", 
							if(!is.null(sset)) paste("WHERE", sset), "group by r.id")
			
			# build table and index	
			dbGetQuery(object@CON, paste("CREATE TABLE" ,tableName, "(", object@ID, "INTEGER,",object@tableName, "NUMERIC)"))
			dbGetQuery(object@CON,paste("CREATE  INDEX", paste(object@tableName, object@ID, sep = "_") ,"ON", tableName, "(id)") )
			dbGetQuery(object@CON, paste("INSERT INTO" ,tableName, richnessSQL) )

		 return(.dbtable.exists(object@CON, tableName))
			} 
	)
		
# agggregate method using sqlite 	 	
setMethod("rangeMapSave",  
	signature  = c(object = "rangeMapSave", FUN = "character", formula = "missing"),
		definition = function(object, FUN, formula, ...) {
		
		# CHECKS
		biotab = paste(object@BIO, object@biotab, sep = "")
			if(!.dbtable.exists(object@CON,biotab) ) 
			stop( paste(sQuote(object@biotab), "is not a table of", sQuote(dbGetInfo(object@CON)$dbname)))
		# object@biotrait should exist as a field in biotab
		if(!.dbfield.exists(object@CON,biotab, object@biotrait) ) 
			stop(paste(sQuote(object@biotrait), "is not a field of", sQuote(object@biotab)))
		
		initExtension(object@CON)

		# fun should  be known by sqlite	
		.sqlAggregate(FUN)
				
		# BIO_tab name
		biotab = paste(object@BIO, object@biotab, sep = "")
		
		#build MAP_ tableName
		tableName = paste(object@MAP, object@tableName, sep = "")
	
		# build sql subset
		sset = .subsetSQLstring(object@CON, object@subset)
		# build sql string
		sql = paste("SELECT r.id, b.",object@biotrait,"FROM ranges r left join ", 
				biotab, " b WHERE r.bioid = b.", .extract.indexed(object@CON, biotab), 
				  if(!is.null(sset)) paste("AND", sset) )
		sql = paste("SELECT id,", FUN ,"(", object@biotrait, ") as", object@biotrait, "from (",sql,") group by id")	

		# build table and index
		dbGetQuery(object@CON, paste("CREATE TABLE" ,tableName, "(", object@ID, "INTEGER,",object@biotrait, "NUMERIC)"))
		dbGetQuery(object@CON, paste("CREATE INDEX", paste(tableName, "id", sep = "_") , "ON", tableName, "(id)") )
		dbGetQuery(object@CON, paste("INSERT INTO" ,tableName, sql) )
		
	
		return(.dbtable.exists(object@CON, tableName))
			
		
		cat(strwrap(sql, width = 100))
		}
	)						

.rangeMapSaveData <-function(object) {
		# CHECKS
		biotab = paste(object@BIO, object@biotab, sep = "")
			if(!.dbtable.exists(object@CON,biotab) ) 
			stop( paste(sQuote(object@biotab), "is not a table of", sQuote(dbGetInfo(object@CON)$dbname)))
		# object@biotrait should exist as a field in biotab
		if(!.dbfield.exists(object@CON,biotab, object@biotrait) ) 
			stop(paste(sQuote(object@biotrait), "is not a field of", sQuote(object@biotab)))
		
		# BIO_tab name
		biotab = paste(object@BIO, object@biotab, sep = "")
		
		#  sql subset
		sset = .subsetSQLstring(object@CON, object@subset)
		#  sql string
		sql = paste("SELECT r.id, b.* FROM ranges r left join ", 
				biotab, " b WHERE r.bioid = b.", .extract.indexed(object@CON, biotab), 
				  if(!is.null(sset)) paste("AND", sset) )

		# fetch table
		d = dbGetQuery(object@CON, sql)
		
		if(nrow(d) == 0) {
			stop( paste("The map is going to be empty! Maybe the bioid in", sQuote(object@biotab), " BIO table was wrongly set.") ) }
		
		
		# return list
		split(d, d[, object@ID])
}
	
	
# agggregate method using R functions called directly on the data 
setMethod("rangeMapSave",  
	signature  = c(object = "rangeMapSave", FUN = "function", formula = "missing"), 
		definition = function(object, FUN, formula, ...) {
		
		# tableName
		tableName = paste(object@MAP, object@tableName, sep = "")
		
		# get data afer checking
		dl = .rangeMapSaveData (object)
		
		# apply R function
		X = sapply(dl, FUN = function(x) FUN(x[, object@biotrait], ...) )
				
		X = data.frame(id = names(X), X)
		names(X) = c(object@ID, object@biotrait)
		row.names(X) = NULL
				
		# build table and index
		dbGetQuery(object@CON, paste("CREATE TABLE" ,tableName, "(", object@ID, "INTEGER,",object@biotrait, "NUMERIC)"))
		dbGetQuery(object@CON, paste("CREATE INDEX", paste(tableName, "id", sep = "_") , "ON", tableName, "(id)") )
		dbWriteTable(object@CON, tableName, X, row.names = FALSE, append = TRUE)
		

		return(.dbtable.exists(object@CON, tableName))
			
		}
	)

# agggregate method using R functions called directly using formula, data interface
setMethod("rangeMapSave",  
	signature  = c(object = "rangeMapSave", FUN = "function", formula = "formula"), 
		definition = function(object, FUN, formula, ...) {
		
		# tableName
		tableName = paste(object@MAP, object@tableName, sep = "")
				
		# get data afer checking
		dl = .rangeMapSaveData (object)
		
		# apply R function
		X = sapply(dl, FUN = function(x) FUN(formula = formula, data = x, ...) )
				
		X = data.frame(id = names(X), X)
		names(X) = c(object@ID, object@biotrait)
		row.names(X) = NULL
				
		# build table and index
		dbGetQuery(object@CON, paste("CREATE TABLE" ,tableName, "(", object@ID, "INTEGER,",object@biotrait, "NUMERIC)"))
		dbGetQuery(object@CON, paste("CREATE INDEX", paste(tableName, "id", sep = "_") , "ON", tableName, "(id)") )
		dbWriteTable(object@CON, tableName, X, row.names = FALSE, append = TRUE)
		

		return(.dbtable.exists(object@CON, tableName))
			
		}
)

# IMPORT RASTER	
	
setGeneric("rangeMapImport", function(object,FUN, ...)  	  standardGeneric("rangeMapImport") )
	
# method for  importing external files
setMethod("rangeMapImport",  
	signature  = c(object = "MapImport", FUN = "function"),
		definition = function(object,FUN, ...) {

	filenam = basename(object@path)
	
	if(length(object@tableName)== 0) tableName = RSQLite::make.db.names(filenam)
	
	tableName = paste(object@MAP, object@tableName, sep = "")		
		
	cnv = canvas.fetch(object@CON)
	message("Converting canvas to polygons...")
	cnv = rasterToPolygons(raster(cnv))
	
	message("Loading external MAP data...")
	rst = raster(object@path)
		
	# is there any other way to compare CRS-s ?	
	if(!CRSargs(CRS(proj4string(cnv))) == CRSargs(projection(rst, FALSE))) 
		warning(sQuote(filenam), " may have a different PROJ4 string;\n", "canvas:", CRSargs(CRS(proj4string(cnv))), "\n", filenam, ":", CRSargs(projection(rst, FALSE)) )
	
	rstp = as(as(rst, "SpatialGridDataFrame"), "SpatialPointsDataFrame") 
	message("Extracting Layer 1...")
	rstp = rstp[which(!is.na(rstp@data[,1])), ]
	
	rstp@data$ptid = as.numeric(rownames(rstp@data)) # add point id
	
	message(paste("Performing overlay: canvas polygons over", filenam, "...") )	
	o = over(rstp, cnv) 
	o$ptid = as.numeric(rownames(o))

	o = merge(o, rstp@data, by = "ptid")
	o$ptid = NULL
	
	message("Agregating data...")
	o = aggregate(o[, 2], list(o[,1]), FUN = FUN, na.rm = TRUE, ...)
	
	names(o) = c(object@ID, object@tableName) 

	# build table and index
	message("Creating table and indexes...")
	dbGetQuery(object@CON, paste("CREATE TABLE" ,tableName, "(", object@ID, "INTEGER,",object@tableName, "FLOAT)"))
	dbGetQuery(object@CON, paste("CREATE INDEX", paste(tableName, "id", sep = "_") , "ON", tableName, "(id)") )
	dbWriteTable(object@CON, tableName, o, row.names = FALSE, append = TRUE)
	
	
	res = .dbtable.exists(object@CON, tableName)
	
	if(res) message(paste(sQuote(basename(object@path)), "imported"))
	
	return(res)	
							
								
		}
	)

	
# user level function calling rangeMapSave
rangeMap.save  <- function(CON, tableName, FUN, biotab, biotrait, subset = list(), path , overwrite = FALSE, ...) {
	
	if(overwrite) 
	try(dbGetQuery(CON, paste("DROP TABLE", paste("MAP", tableName, sep = "_"))), silent = TRUE)
		
	if(!missing(path)) { #  external map
			if(missing(tableName))
				rmap = new("MapImport", CON = CON, path = path) else
				rmap = new("MapImport", CON = CON, path = path, tableName = tableName)
			 
			 rangeMapImport(rmap, FUN = FUN)
			} 
	
	if(missing(FUN) ) { #species richness
			if(missing(tableName))
				rmap = new("rangeMapSave", CON = CON, subset = subset) else
				rmap = new("rangeMapSave", CON = CON, tableName = tableName, subset = subset)
			 rangeMapSave(rmap)	
			
			} 
	
	if(!missing(FUN) & missing(path)) { # SQL or R function
			rmap = new("rangeMapSave", CON = CON,  biotab = biotab, biotrait = biotrait, tableName = tableName, subset = subset)
			 rangeMapSave(rmap, FUN = FUN, ...)	
			}
	


}				



















