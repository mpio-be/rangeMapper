#' @rdname rangeMap.start
rangeMap <- function(path){
	new("rangeMap", CON = dbConnect(RSQLite::SQLite(), dbname= path) )
	}

setMethod("show", signature(object = "rangeMap"), function(object){
	out = list()
	tbs = dbGetQuery(object@CON, "select name from sqlite_master where type = 'table' ")$name

	out[["class"]] = class(object)
	dbinfo = dbGetInfo(object@CON)
	out[["Project_location"]] = object@CON@dbname
	out[["SQLite_version"]]   = dbinfo$serverVersion

	if( is.empty(object@CON, object@CANVAS) )
	out[["empty_project"]]  = "Empty rangeMapper project." else {
		out[["Proj4"]]      = dbReadTable(object@CON, object@PROJ4STRING)[1,1]
		out[["CellSize"]]   = dbReadTable(object@CON, object@GRIDSIZE)[1,1] %>% prettyNum
		Extent			    = as.list(dbReadTable(object@CON, object@BBOX))
		out[["Extent"]]     = Extent  %>% prettyNum %>%
							  paste(names(Extent), ., sep  = '=') %>%
							  paste(.,collapse =',')
		out[["BIO_tables"]] = paste(  gsub(object@BIO, "", tbs[grep(object@BIO, tbs)]), collapse = ";" )
		out[["MAP_tables"]] = paste(  gsub(object@MAP, "", tbs[grep(object@MAP, tbs)]), collapse = ";" )
		mtd = is.empty(object@CON, object@METADATA_RANGES)

		out[["METADATA_RANGES"]] = dbGetQuery(object@CON, paste("select * from", object@METADATA_RANGES, "limit 1") ) %>%
									names  %>% setdiff(., 'bioid') %>%
									paste(.,collapse =',')

      }

     names(out)  = sprintf(paste0('%-', max(nchar(names(out))), 's'), names(out))


     delim = paste0('+', rep('_', nchar(names(out)[1])-2) %>%
     		  paste0(., collapse = ''), '+',collapse = '')
     message(delim)
     paste(names(out), out) %>%
     paste(., collapse = "\n")  %>%
     message
     message(delim)

     })

setGeneric("rangeMapStart", function(object)	standardGeneric("rangeMapStart") )

setMethod("rangeMapStart", signature  = "rangeMapStart", definition = function(object){
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

#' Initiate/open a new rangeMapper project
#'
#' @param dir       Project directory.
#' @param file      Project's file name.
#' @param overwrite Logical vector, default to FALSE (the file is kept but all tables are dropped).
#' @param path      Character vector; a path to a valid rangeMapper project.
#' @param verbose   Character vector; if \code{TRUE} the project's summary is printed.
#' @return          rangeMap.start() and rangeMap.open() returns an sqlite connection\cr
#'					rangeMap() returns an object of class  \code{rangeMap}
#' @seealso         \code{\link{rangeMap.save}}
#' @export          rangeMap.start rangeMap.open rangeMap
#' @examples
#'
#' td = setwd(tempdir())
#'
#' dbcon = rangeMap.start(file = "test.sqlite", overwrite = TRUE, dir = tempdir() )
#' summary(dbcon)
#'
#' summary(rangeMap("test.sqlite"))
#'
#' dbcon = rangeMap.open(path = "test.sqlite")
#' summary(dbcon)
#' setwd(td)
#'
#'
rangeMap.start <- function(dir, file, overwrite = FALSE) {

	obj = new("rangeMapStart", dir = dir, file = file, overwrite = overwrite)

	rangeMapStart(obj)
	message( paste("New session", Sys.time() ) )
	message(paste("PROJECT:", obj@file, "\nDIRECTORY:",obj@dir) )

	f = paste(obj@dir, obj@file, sep = .Platform$file.sep)
	invisible(dbConnect( RSQLite::SQLite() , dbname = f))
	}

#' @rdname rangeMap.start
rangeMap.open <- function(path, verbose = TRUE) {

	stopifnot(file.exists(path))

	dbcon = dbConnect( RSQLite::SQLite() , dbname= path)

	o = new("rangeMap", CON = dbcon)

	if(verbose) show(o)

	invisible(dbcon)
	}
