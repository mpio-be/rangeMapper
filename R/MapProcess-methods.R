#' processRanges
#'
#' @param con       a \code{connection} object.
#' @param spdf    	\code{\link[sp]{SpatialPolygonsDataFrame}} object containing all the ranges.
#' @param dir     	ranges file directory where the individual ranges shp files are located. In this case the range ID is the file name.
#' @param ID      	a character vector of length one. An \code{spdf} column name indicating the range ID (e.g. species name).
#' @param metadata 	a named list of functions. See \code{\link{rangeTraits}} and \code{\link{metadata.update}}.
#' @param parallel 	default to FALSE. When TRUE a \code{multiprocess} evaluation scheme is initiated.
#' 
#' @export
#' @examples
#' require(rangeMapper)
#' require(rgdal)
#'
#' dbcon = rangeMap.start(file = "wrens.sqlite", dir = tempdir(), overwrite = TRUE)
#' f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
#' r = readOGR(f, "wrens", verbose = FALSE)[1:50, ]
#' global.bbox.save(con = dbcon, bbox = r)
#' gridSize.save(dbcon, gridSize = 5)
#' canvas.save(dbcon)
#' processRanges(con = dbcon, spdf = r, ID = "sci_name", metadata = rangeTraits() )
#' dbDisconnect(dbcon)
#' 

setGeneric("processRanges",
	function(con,spdf, dir, ID,metadata,parallel)
	standardGeneric("processRanges") )

#' @describeIn  processRanges Method 1: One SpatialPolygonsDataFrame containing all the ranges. No metadata.
setMethod("processRanges",
	signature = c(con        = "SQLiteConnection",
							 spdf        = "SpatialPolygonsDataFrame",
							 dir         = "missing",
							 ID          = "character",
							 metadata    = "missing" 
							),
	definition = function(con, spdf, ID,  metadata,parallel){

	Startprocess = Sys.time()
	# ini
		if(missing(parallel)) parallel = FALSE

		if (!parallel) {
			future::plan(future::transparent)
			} else {
			doFuture::registerDoFuture()
			future::plan(future::multiprocess)
			}



		ranges.exists(con)
		rmo = new("rangeMap", CON = con)

		td = tempfile('processRanges_')
		dir.create(td)
		wd = getwd()

	# Elements
		cnv = canvas.fetch(con) %>% as(., "SpatialPointsDataFrame")
		p4s =  dbReadTable(con, "proj4string")[1,1] # TODO: write get function
		spdf = spdf[, ID]
		names(spdf) = 'ID'

	#  Reproject spdf to p4s
		if( ! proj4string_is_identical(proj4string(spdf), p4s) ) {
			warning( paste("Reprojecting to", dQuote(p4s) ) )
			spdf = spTransform( spdf , CRS(p4s, doCheckCRSArgs = FALSE) )
			}

	# range over canvas

		foreach(i = as.character(spdf$ID) , .packages = c('rangeMapper') ) %dopar% {
 			spi  = spdf[spdf$ID == i, ]
 			oi   = rangeOverlay( spi , cnv, i)
 			setwd(td)
 			write.table(oi, file = i , sep = ",", quote = FALSE, row.names = FALSE)
			}

		setwd(wd)

	# db update
		message("Writing overlay output to project...")

		inc = list.files(td, full.names = TRUE)

		res = sapply(inc, FUN = function(f)  dbWriteTable(con, rmo@RANGES, fread(f), append = TRUE, row.names = FALSE) )

		if(any(res)) message(sum(res), " out of ", length(spdf$ID) , " ranges updated; Elapsed time: ",
						round(difftime(Sys.time(), Startprocess, units = "mins"),1), " mins")

	})

#' @describeIn  processRanges Method 2: One SpatialPolygonsDataFrame containing all the ranges. Metadata are computed.
setMethod("processRanges",
	signature = c(con        = "SQLiteConnection",
				 spdf        = "SpatialPolygonsDataFrame",
				 dir         = "missing",
				 ID          = "character",
				 metadata    = "list"),
	definition = function(con, spdf, ID,  metadata,parallel){

	# ini
		if(missing(parallel)) parallel = FALSE

		if (!parallel) {
			future::plan(future::transparent)
			} else {
			doFuture::registerDoFuture()
			future::plan(future::multiprocess)
			}

		ranges.exists(con)
		rmo = new("rangeMap", CON = con)

	# Elements
		p4s =  dbReadTable(con, "proj4string")[1,1]
		ids = spdf@data[, ID]

	# 1. process ranges
		processRanges(con = con, spdf = spdf, ID = ID)

	# 2. process metadata
		message("Extracting metadata...")
		rtr = foreach(i = ids, .packages = 'sp', .combine = rbind) %dopar%  {
			spi = spdf[spdf@data[, ID] == i, ]
			oi  = sapply(metadata, function(x) x(spi ) ) %>% t %>% data.frame
			cbind(bioid = i[1], oi)
			}

		# prepare sql statements
		st = data.frame(cols = names(rtr), rtypes = sapply(rtr, typeof), stringsAsFactors = FALSE)
		st = st[-1, ] # the bioid column is written at project ini
		st$sqltypes = sapply(st$rtypes, function(x)
				switch(x, double = 'NUMERIC',
						 integer = 'INTEGER',
						 logical = 'INTEGER',
						 character = 'TEXT') )
		st$sql = paste("ALTER TABLE metadata_ranges ADD COLUMN", st$cols, st$sqltypes)

		# prepare metadata table
		sapply(st$sql, dbExecute, conn =  con)
		# save
		dbWriteTable(con, rmo@METADATA_RANGES, rtr, append = TRUE, row.names = FALSE)




	})

#' @describeIn processRanges Method 3: Each range file is a separate shp file. No metadata.
setMethod("processRanges",
	signature = c(con        = "SQLiteConnection",
				  spdf       = "missing",
				  dir        = "character",
				  ID         = "missing",
				  metadata   = "missing"),
	definition = function(con, dir,parallel){

	Startprocess = Sys.time()

	# ini
		if(missing(parallel)) parallel = FALSE

		if (!parallel) {
			future::plan(future::transparent)
			} else {
			doFuture::registerDoFuture()
			future::plan(future::multiprocess)
			}

		ranges.exists(con)
		rmo = new("rangeMap", CON = con)

	# Elements
		Files = rangeFiles(new("rangeFiles", dir = dir))
		cnv = canvas.fetch(con) %>% as(., "SpatialPointsDataFrame")
		p4s =  dbReadTable(con, "proj4string")[1,1]

	# Range over canvas
		roc = foreach(i = 1:nrow(Files), .packages = c('sp') ) %dopar% {
			ri = readOGR(Files[i,'dsn'], Files[i,'layer'], verbose = FALSE)

			if( ! proj4string_is_identical(proj4string(ri), p4s) ) {
				ri = spTransform( ri , CRS(p4s,doCheckCRSArgs = FALSE) )
				}

			oi = rangeOverlay(ri,  cnv, Files[i,'layer'])

			names(oi) = c(rmo@ID, rmo@BIOID)

			# save  to db
			res = dbWriteTable(con, rmo@RANGES, oi, append = TRUE, row.names = FALSE)
			}

	# Msg
		message(paste(  unlist(roc) %>% sum , "ranges updated to database; Elapsed time:",
			round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins") )
	})

#' @describeIn processRanges Method 4: Each range file is a separate shp file. Metadata are computed.
setMethod("processRanges",
	signature = c(con        = "SQLiteConnection",
				  spdf       = "missing",
				  dir        = "character",
				  ID         = "missing",
				  metadata   = "list"),
	definition = function(con, dir, metadata,parallel){

	Startprocess = Sys.time()

	# ini
		if(missing(parallel)) parallel = FALSE

		if (!parallel) {
			future::plan(future::transparent)
			} else {
			doFuture::registerDoFuture()
			future::plan(future::multiprocess)
			}

		ranges.exists(con)
		rmo = new("rangeMap", CON = con)

	# Elements
		Files = rangeFiles(new("rangeFiles", dir = dir))
		cnv = canvas.fetch(con) %>% as(., "SpatialPointsDataFrame")
		p4s =  dbReadTable(con, "proj4string")[1,1]

	# prepare metadata table (on first range)
		sp1  = readOGR(Files[1,'dsn'], Files[1,'layer'], verbose = FALSE)
		rtr1 = sapply(metadata, function(x) x(sp1 ) ) %>% t %>% data.frame
		rtr1 = cbind(bioid = Files[1,'layer'], rtr1)


		# prepare sql statements
		st = data.frame(cols = names(rtr1), rtypes = sapply(rtr1, typeof), stringsAsFactors = FALSE)
		st = st[-1, ]
		st$sqltypes = sapply(st$rtypes, function(x)
				switch(x, double = 'NUMERIC',
						 integer = 'INTEGER',
						 logical = 'INTEGER',
						 character = 'TEXT') )
		st$sql = paste("ALTER TABLE metadata_ranges ADD COLUMN", st$cols, st$sqltypes)

		# prepare metadata table
		sapply(st$sql, dbExecute, conn =  con)
		# save 1st range metadata
		dbWriteTable(con, rmo@METADATA_RANGES, rtr1, append = TRUE, row.names = FALSE)


	# Range over canvas
		roc = foreach(i = 2:nrow(Files), .packages = c('sp') ) %dopar% {
			spi = readOGR(Files[i,'dsn'], Files[i,'layer'], verbose = FALSE)

			if( ! proj4string_is_identical(proj4string(spi), p4s) ) {
				spi = spTransform( spi , CRS(p4s, doCheckCRSArgs = FALSE) )
				}
			# do overlay
			oi = rangeOverlay(spi,  cnv, Files[i,'layer'])
			names(oi) = c(rmo@ID, rmo@BIOID)

			# metadata
			mi  = sapply(metadata, function(x) x(spi ) ) %>% t %>% data.frame
			mi = cbind(bioid = i[1], mi)

			# save
			res1 = dbWriteTable(con, rmo@RANGES, oi, append = TRUE, row.names = FALSE)
			res2 = dbWriteTable(con, rmo@METADATA_RANGES, mi, append = TRUE, row.names = FALSE)
			all(res1, res2)
			}

	# Msg
		message(paste(  unlist(roc) %>% sum +1 , "ranges updated to database; Elapsed time:",
			round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins") )
	})
