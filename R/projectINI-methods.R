
setGeneric("rangeMapStart", function(object, ...)	standardGeneric("rangeMapStart") )

setMethod("rangeMapStart",  
	signature = "rangeMapStart", 
	definition = function(object){
	f = paste(object@dir, object@file, sep = .Platform$file.sep)
	file.exists = if(file.exists(f) )  TRUE else FALSE
	CON = dbConnect(RSQLite::SQLite(), dbname = f)
	verSql = paste("INSERT INTO version VALUES (" ,shQuote(packageDescription("rangeMapper")$Version) , ")")

	if(!file.exists) { 				
		Queries = object@skeleton
		db = unlist(Queries)
			for (i in 1:length(db)) 
					dbGetQuery(CON , db[i])

		dbGetQuery(CON, verSql)
		}

	if(object@overwrite && file.exists) {
		dropAll = dbGetQuery(CON, "select 'drop table if exists ' || name from sqlite_master where type = 'table';")
		if(nrow(dropAll) == 0) dropAll = NULL else dropAll = dropAll[,1 ]
		Queries = c(dropAll, "vacuum", object@skeleton )
		db = unlist(Queries)
			for (i in 1:length(db)) dbGetQuery(CON , db[i])
		
		dbGetQuery(CON, verSql)
		}

	if(!object@overwrite && file.exists) stop(paste("File", object@file, "already exists!"))


	})

# user level functions 

rangeMap <- function(path){
	new("rangeMap", CON = dbConnect(RSQLite::SQLite(), dbname= path) )
	}

rangeMap.start <- function(...) {

	obj = new("rangeMapStart", ... )
	
	rangeMapStart(obj)
	message( paste("New session", Sys.time() ) )
	message(paste("PROJECT:", obj@file, "\nDIRECTORY:",obj@dir) )
	
	f = paste(obj@dir, obj@file, sep = .Platform$file.sep)
	invisible(dbConnect( RSQLite::SQLite() , dbname = f))
	}

rangeMap.open <- function(path, verbose = TRUE) {
	
	stopifnot(file.exists(path))
	
	dbcon = dbConnect( RSQLite::SQLite() , dbname= path)
	
	o = new("rangeMap", CON = dbcon)
	
	if(verbose) summary(o)
	
	invisible(dbcon)
	}
	