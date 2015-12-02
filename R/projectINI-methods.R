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

#' Initiate/open a new rangeMapper project
#'
#' Initiate/open a new rangeMapper project using a
#' \code{\link{rangeMapStart-class} object}
#'
#'
#' @aliases rangeMap.start rangeMapStart rangeMap.open rangeMap
#' @param path Character vector; a path to a valid rangeMapper project
#' @param verbose Character vector; if \code{TRUE} the project's summary is
#' printed
#' @param \dots Arguments to be passed to \code{\link{rangeMapStart-class}}
#' @return rangeMap.start() and rangeMap.open() returns an sqlite connection.
#' rangeMap() returns a \code{\link{rangeMap-class}} object.
#' @author Mihai Valcu \email{valcu@@orn.mpg.de}
#' @seealso \code{\link{rangeMap.save}}.\cr \code{\link{rangeMapStart-class}}
#' @keywords spatial
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
#' @export rangeMap.start
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

	if(verbose) print(summary(o))

	invisible(dbcon)
	}
