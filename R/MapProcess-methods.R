#' processRanges
#'
#' @param sqlitecon a \code{connection} object.
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
#' processRanges(sqlitecon = dbcon, spdf = r, ID = "sci_name")
#' stopCluster(cl)

setGeneric("processRanges",
	function(sqlitecon,spdf, dir, ID,metadata)
	standardGeneric("processRanges") )

#' @describeIn  processRanges One SpatialPolygonsDataFrame containing all the ranges, no metadata are not computed
setMethod("processRanges",
	signature = c(sqlitecon  = "SQLiteConnection",
				 spdf        = "SpatialPolygonsDataFrame",
				 dir         = "missing",
				 ID          = "character",
				 metadata    = "missing"),

	definition = function(sqlitecon, spdf, ID,  metadata){

	Startprocess = Sys.time()

	# ini
		rmo = new("rangeMap", CON = sqlitecon)
		if(!is.empty(rmo@CON, rmo@RANGES))
			stop(paste(dQuote(rmo@RANGES), "table is not empty!"))

	# Elements
		cnv = canvas.fetch(sqlitecon) %>% as(., "SpatialPointsDataFrame")
		p4s =  dbReadTable(sqlitecon, "proj4string")[1,1] # TODO: write get function
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
 			spi = as(spi, 'SpatialPolygons')

 			rangeOverlay( spi , cnv, nami)
			}

	# db update
		message("Writing to project.")
		res = dbWriteTable(sqlitecon, "ranges", roc, append = TRUE, row.names = FALSE)


		if(res) message(paste(length(unique(spdf$ID)) , "ranges updated; Elapsed time:",
						round(difftime(Sys.time(), Startprocess, units = "secs"),1), "secs") )

	})

#' @describeIn  processRanges One SpatialPolygonsDataFrame containing all the ranges, metadata are computed
setMethod("processRanges",
	signature = c(sqlitecon  = "SQLiteConnection",
				 spdf        = "SpatialPolygonsDataFrame",
				 dir         = "missing",
				 ID          = "character",
				 metadata    = "list"),

	definition = function(sqlitecon, spdf, ID,  metadata){

	# ini
		rmo = new("rangeMap", CON = sqlitecon)

	# Elements
		p4s =  dbReadTable(sqlitecon, "proj4string")[1,1]
		ids = spdf@data[, ID]


	# 1. process ranges
		processRanges(sqlitecon = sqlitecon, spdf = spdf, ID = ID)

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
				function(x)  dbGetQuery(sqlitecon, x))


		res = dbWriteTable(sqlitecon, rmo@METADATA_RANGES, rtr, append = TRUE, row.names = FALSE)




	})

