
brewer.pal.get <- function(palette = NULL) {
	pal = brewer.pal.info
	pal = pal[!pal$category == "qual",]
	bp = lapply(split(pal, row.names(pal)), FUN = function(x) brewer.pal(x$maxcolors, row.names(x)))
	if(!is.null(palette) && palette%in%names(bp) ) bp = bp[palette][[1]]
	bp 		   
	}

.extract.indexed <-function(con,table.name) {
	# extract name of indexed colum
	indx = dbGetQuery(con, 
		paste("select * from sqlite_master where type = 'index' and tbl_name = '", 
				table.name, "'", sep = ""))$name
	if(length(indx)  == 0)	res = NA else	
	res = dbGetQuery(con, paste("PRAGMA index_info(",indx, ")" ))$name
	res
	}

.dbtable.exists <- function(con, table.name) {
	# returns TRUE if the table exists on channel 
	x = dbGetQuery(con,paste('select name from sqlite_master where type in ("table") and tbl_name like', shQuote(table.name) ) )
	if(nrow(x)>0) TRUE else FALSE
	
	}

.dbfield.exists <- function(con, table.name, col.name) {
	# returns TRUE if the column is part of table
	stopifnot(.dbtable.exists(con, table.name))
	
	ans = length(intersect(dbGetQuery(con, paste("pragma table_info(", table.name, ")") )$name, col.name)) > 0
	ans
	}	
	
.is.empty <- function(con, table.name) {
	# returns TRUE if table is  empty FALSE otherwise
	# performs a SELECT * from table limit 1;

	res = dbGetQuery(con, paste("SELECT * from", table.name, "limit 1") )
	if(nrow(res) == 0) TRUE else 
		FALSE
	} 

.sqlAggregate <- function(fun){
 # list of sql aggregate functions
 # If fun is given checks for its existence else return the list of sqlite aggregate functions
	funs = list(
	avg  		  = "avg", 
	stdev         = "stdev",
	variance      = "variance",
	mode          = "mode",
	median        = "median",
	lower_quartile= "lower_quartile",
	upper_quartile= "upper_quartile",
	sum           = "total",
	max           = "max",
	min           = "min",
	count         = "count")
	 
	class(funs) = "simple.list"

	if(missing(fun) )
	 return(funs) else if
		(fun%in%funs) return(TRUE) else
				stop(sQuote(fun), "is not a known sqlite aggregate function!")
	}

.dbRemoveField <- function(con, table.name, col.name) {
	# table def (type and indexes)
	tinfo = dbGetQuery(con, paste("pragma table_info(" , shQuote(table.name),")" ))
	
	if( is.element(col.name, tinfo$name) ) {
		tinfo = tinfo[tinfo$name != col.name, ]
		indexSQL = dbGetQuery(con, paste("select * from sqlite_master where type = 'index' and tbl_name = '", table.name, "'", sep = ""))$sql
		# do ALTER, CREATE, INSERT FROM SELECT, DROP
		dbBeginTransaction(con)
		dbSendQuery(con, paste("ALTER TABLE" ,table.name, "RENAME TO temptab") )
		dbSendQuery(con, paste("CREATE TABLE" ,table.name, '(', paste(tinfo$name, tinfo$type, collapse = ',') , ')'))
		dbSendQuery(con, paste("INSERT INTO" ,table.name, 'SELECT ', paste(tinfo$name, collapse = ',') , 'FROM temptab'))
		dbSendQuery(con, " DROP TABLE temptab")
		if(length(indexSQL > 1)) lapply(indexSQL, function(x) try(dbGetQuery(con, x), silent = TRUE) )
		
		dbCommit(con)
		} else FALSE 

	}
















