
setGeneric("assemblageFetch", function(object, xy, BIO)		standardGeneric("assemblageFetch") )

setMethod("assemblageFetch",  
	signature  = c(object = "rangeMap", xy = "SpatialPoints", BIO = "missing"), 
		definition = function(object, xy) {
		
		# CANVAS
		cnv = canvas.fetch(object@CON)

		#Assembladge IDs
		assembl_id = over(xy, cnv)$id

		# buid sql 
		sql = paste("SELECT * FROM ranges WHERE id in(",	paste(assembl_id, collapse = ",")  ,")")

		#fetch assambladges
		A = dbGetQuery(object@CON, sql)
		
		return(A)
		}
)

setMethod("assemblageFetch",  
	signature  = c(object = "rangeMap", xy = "SpatialPoints", BIO = "character"), 
		definition = function(object, xy, BIO) {
		
		# CANVAS
		cnv = canvas.fetch(object@CON)

		# BIO_table
		biotabs = dbGetQuery(object@CON, "SELECT * FROM sqlite_master WHERE type='table' and name like 'BIO_%' ")$name
		if(BIO%in%biotabs) stop(paste(dQuote(BIO), "is not a BIO_table"))
		BIO = paste("BIO", BIO, sep = "_") 
		biotab_id = .extract.indexed(object@CON, BIO)

		#Assembladge IDs
		assembl_id = over(xy, cnv)$id

		# buid sql 
		sql = paste("SELECT * FROM ranges r LEFT JOIN", BIO, "ON", paste(BIO,biotab_id, sep = ".") , "= r.bioid", "WHERE r.id in(",	paste(assembl_id, collapse = ",")  ,")")

		#fetch assambladges
		A = dbGetQuery(object@CON, sql)
		A$bioid = NULL
		
		return(A)
		}
)	
	
	
	
	




















