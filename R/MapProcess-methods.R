#' processRanges
#'
#' @param con a \code{connection} object.
#' @param spdf    	\code{\link[sp]{SpatialPolygonsDataFrame}} object containing all the ranges.
#' @param dir     	ranges file directory where the individual ranges shp files are located. In this case the range ID is the file name.
#' @param ID      	when spdf is set this is a \code{character} vector given the name of the range.
#' @param metadata 	a named list of functions. See \code{\link[rangeMapper]{rangeTraits}} and \code{\link[rangeMapper]{metadataUpdate}}
#' @export
#' @examples
#' require(rangeMapper)
#' if (require(doParallel) ) {
#'  cl = makePSOCKcluster(detectCores())
#'  registerDoParallel(cl) }
#'
#' dbcon = rangeMap.start(file = "wrens.sqlite", dir = tempdir(), overwrite = TRUE)
#' f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
#' global.bbox.save(con = dbcon, bbox = f, p4s = CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
#' gridSize.save(dbcon, gridSize = 250000)
#' canvas.save(dbcon)
#'
#' r = readOGR(f, "wrens", verbose = FALSE)
#' processRanges(con = dbcon, spdf = r, ID = "sci_name")
#' stopCluster(cl)

setGeneric("processRanges",
	function(con,spdf, dir, ID,metadata)
	standardGeneric("processRanges") )

#' @describeIn  processRanges One SpatialPolygonsDataFrame containing all the ranges. No metadata.
setMethod("processRanges",
	signature = c(con        = "SQLiteConnection",
				 spdf        = "SpatialPolygonsDataFrame",
				 dir         = "missing",
				 ID          = "character",
				 metadata    = "missing"),

	definition = function(con, spdf, ID,  metadata){

	Startprocess = Sys.time()

	# ini
		rmo = new("rangeMap", CON = con)
		if(!is.empty(rmo@CON, rmo@RANGES))
			stop(paste(dQuote(rmo@RANGES), "table is not empty!"))

	# Elements
		cnv = canvas.fetch(con) %>% as(., "SpatialPointsDataFrame")
		p4s =  dbReadTable(con, "proj4string")[1,1] # TODO: write get function
		spdf = spdf[, ID]
		names(spdf) = 'ID'

	#  Reproject spdf to p4s
		if( ! proj4string_is_identical(proj4string(spdf), p4s) ) {
			warning( paste("Reprojecting to", dQuote(p4s) ) )
			spdf = spTransform( spdf , CRS(p4s) )
			}

	# range over canvas
		roc = foreach(i = spdf$ID, .packages = c('sp'), .combine = rbind) %dopar% {
 			spi  = spdf[spdf$ID == i, ]
 			nami = spi$ID[1]
 			rangeOverlay( spi , cnv, nami)
			}

	# db update
		message("Writing to project.")
		res = dbWriteTable(con, "ranges", roc, append = TRUE, row.names = FALSE)


		if(res) message(paste(length(unique(spdf$ID)) , "ranges updated; Elapsed time:",
						round(difftime(Sys.time(), Startprocess, units = "secs"),1), "secs") )

	})

#' @describeIn  processRanges One SpatialPolygonsDataFrame containing all the ranges. Metadata are computed.
setMethod("processRanges",
	signature = c(con  = "SQLiteConnection",
				 spdf        = "SpatialPolygonsDataFrame",
				 dir         = "missing",
				 ID          = "character",
				 metadata    = "list"),

	definition = function(con, spdf, ID,  metadata){

	# ini
		rmo = new("rangeMap", CON = con)

	# Elements
		p4s =  dbReadTable(con, "proj4string")[1,1]
		ids = spdf@data[, ID]


	# 1. process ranges
		processRanges(con = con, spdf = spdf, ID = ID)

	# 2. process metadata
		Startprocess = Sys.time()

		message("Extracting metadata...")
		rtr = foreach(i = ids, .packages = 'sp', .combine = rbind) %dopar%  {
			spi = spdf[spdf@data[, ID] == i, ]
			oi  = sapply(metadata, function(x) x(spi ) ) %>% t %>% data.table
			oi[, . := i[1]]
			}
		setnames(rtr, '.', rmo@BIOID)

		  lapply(
			paste("ALTER TABLE metadata_ranges ADD COLUMN", names(rtr)[-ncol(rtr)], "FLOAT"),
				function(x)  dbGetQuery(con, x))


		res = dbWriteTable(con, rmo@METADATA_RANGES, rtr, append = TRUE, row.names = FALSE)




	})

#' @describeIn processRanges each range file is a separate shp file. No metadata.
setMethod("processRanges",
	signature = c(con  = "SQLiteConnection",
				  spdf       = "missing",
				  dir        = "character",
				  ID         = "missing",
				  metadata   = "missing"),
	definition = function(con, dir){

	Startprocess = Sys.time()


	# ini
		rmo = new("rangeMap", CON = con)
		if(!is.empty(rmo@CON, rmo@RANGES))
			stop(paste(dQuote(rmo@RANGES), "table is not empty!"))

	# Elements
		Files = rangeMapper:::rangeFiles(new("rangeFiles", dir = dir))
		cnv = canvas.fetch(con) %>% as(., "SpatialPointsDataFrame")
		p4s =  dbReadTable(con, "proj4string")[1,1]

	# Range over canvas
		roc = foreach(i = 1:nrow(Files), .packages = c('sp') ) %do% {
				ri = readOGR(Files[i,'dsn'], Files[i,'layer'], verbose = FALSE)

				if( ! rangeMapper:::proj4string_is_identical(proj4string(ri), p4s) ) {
					ri = spTransform( ri , CRS(p4s) )
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

