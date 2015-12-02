#TODO: document parallel
#TODO replace .processRange with visible function

#' rangeMapProcess
#'
#' @param object   a \code{rangeMapProcess} object.
#' @param spdf    	\code{\link[sp]{SpatialPolygonsDataFrame}} object containing all the ranges.
#' @param dir     	ranges file directory where the individual ranges shp files are located. In this case the range ID is the file name.
#' @param ID      	when spdf is set this is a \code{character} vector given the name of the range.
#' @param metadata 	a named list of functions. See \code{\link[rangeMapper]{rangeTraits}} and \code{\link[rangeMapper]{metadataUpdate}}
#' @param parallel 	logical, default to false.
#'
#' @seealso \code{\link[rangeMapper]{processRanges}}

setGeneric("rangeMapProcess",
	function(object,spdf, dir, ID,metadata, parallel)
	standardGeneric("rangeMapProcess") )

#' @rdname rangeMapProcess
#' @description Method 1: each range file is a separate shp file. No metadata
setMethod("rangeMapProcess",
	signature = c(object = "rangeMapProcess",spdf = "missing", dir = "character", ID = "missing", metadata = "missing", parallel = "missing"),
	definition = function(object, dir){

	Startprocess = Sys.time()

	Files = rangeFiles(new("rangeFiles", dir = dir))
	cnv = as(canvasFetch(object), "SpatialPointsDataFrame")


	.processRange = function(i) {
		name = Files$layer[i]
		r = readOGR(Files$dsn[i], Files$layer[i], verbose = FALSE)
		#  reproject
		p4s =  dbReadTable(object@CON, object@PROJ4STRING)[1,1]
		if(!identical(gsub(" ", "", proj4string(r)), gsub(" ", "", p4s) ) ) r = spTransform( r , CRS(p4s) )

		# progress report
		message( paste("Processsing ranges, please wait!...",
				   paste("Range:", Files$layer[i]),
					 paste(round(i/length(Files$layer)*100,2), "% done"),
					   paste("Elapsed time:",round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins"), sep = "\n")
					 )

		o = rangeOverlay(r,  cnv, name)

		names(o) = c(object@ID, object@BIOID)

		# save  to db
		dbWriteTable(object@CON, object@RANGES, o, append = TRUE, row.names = FALSE)

		}

	lapply (1:length(Files$layer), FUN = .processRange)


	# last Msg
	message(paste(nrow(Files), "ranges updated to database; Elapsed time:",round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins") )
	})

#' @rdname rangeMapProcess
#' @description Method 2: Each range file is a separate shp file. Metadata are computed
setMethod("rangeMapProcess",
	signature = c(object = "rangeMapProcess",spdf = "missing", dir = "character", ID = "missing", metadata = "list", parallel = "missing"),
	definition = function(object, dir, metadata){


	Startprocess = Sys.time()

	Files = rangeFiles(new("rangeFiles", dir = dir))

	cnv = as(canvasFetch(object), "SpatialPointsDataFrame")


	.processRange = function(i) {

	name = Files$layer[i]
	r = readOGR(Files$dsn[i], Files$layer[i], verbose = FALSE)

	#  reproject
	p4s =  dbReadTable(object@CON, object@PROJ4STRING)[1,1]
	if(!identical(gsub(" ", "", proj4string(r)), gsub(" ", "", p4s) ) ) r = spTransform( r , CRS(p4s) )


	# progress report
	message( paste("Processsing ranges, please wait!...",
			   paste("Range:", Files$layer[i]),
				 paste(round(i/length(Files$layer)*100,2), "% done"),
				   paste("Elapsed time:",round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins"), sep = "\n")
				)


	o = rangeMapper::rangeOverlay(r,  cnv, name)

	names(o) = c(object@ID, object@BIOID)

	# save  to @RANGES
	dbWriteTable(object@CON, object@RANGES, o, append = TRUE, row.names = FALSE)

	# save  to @METADATA_RANGES
	rtr = sapply(metadata, function(x) x(r) )

	rtr = data.frame(t(rtr))
	id = data.frame(name); names(id) = object@BIOID
	metadata = cbind(id, rtr)

	 if(i == 1) {
	  lapply(
		paste("ALTER TABLE metadata_ranges ADD COLUMN", names(rtr[, 1:ncol(rtr), drop = FALSE]), "FLOAT"),
			function(x)  dbGetQuery(object@CON, x)) }

	dbWriteTable(object@CON, object@METADATA_RANGES, metadata, append = TRUE, row.names = FALSE)
	}


	lapply (1:length(Files$layer), FUN = .processRange)

	# last Msg
	message(paste(nrow(Files), "ranges updated to database; Elapsed time:",
					round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins"), keep = TRUE )
	})

#' @rdname rangeMapProcess
#' @description Method 3: One shp file containing all ranges, ID is required. No metadata
setMethod("rangeMapProcess",
	signature = c(object = "rangeMapProcess",spdf = "SpatialPolygonsDataFrame", dir = "missing", ID = "character", metadata = "missing", parallel = "logical"),
	definition = function(object, spdf, ID,  metadata, parallel){

	Startprocess = Sys.time()

	if(parallel) {
		library(doParallel)
		cl = makePSOCKcluster(detectCores())
		registerDoParallel(cl)
		on.exit( stopCluster(cl) )
		}

	message("Processsing ranges, please wait!...")

	cnv = as( canvasFetch(object), "SpatialPointsDataFrame")

	#  reproject
	p4s =  dbReadTable(object@CON, object@PROJ4STRING)[1,1]
	if(!identical(gsub(" ", "", proj4string(spdf)), gsub(" ", "", p4s) ) ) {
  	warning( paste("Reprojecting to", dQuote(p4s)), keep = FALSE)
  	spdf = spTransform( spdf , CRS(p4s) )
  	}

	# TODO: change to foreach. subset range instead of split.
	
	# split by range
	message( "Identifing ranges...")
	spdf = split(spdf, spdf@data[, ID])
	message( paste(length(spdf), " ranges found.") )

	rnames = names(spdf)

	message( "Processing ranges...")
	.processRange = function(x) {
		name = x@data[1, ID]
		pos = which(rnames%in%name)
		if(!parallel) setTxtProgressBar(pb, pos)
		 rangeOverlay(x,  cnv, name)
		}


	if(!parallel) {
		pb = txtProgressBar(min = 0, max = length(rnames), char = ".", style = 3)
		overlayRes = lapply(spdf, .processRange)
		close(pb)
		}

	if(parallel)
		overlayRes = foreach(i = 1:length(spdf), .packages = 'rangeMapper') %dopar%  .processRange(spdf[[i]])


	overlayRes = do.call(rbind, overlayRes)

	names(overlayRes) = c(object@ID, object@BIOID)

	message("Writing to project.")
	res = dbWriteTable(object@CON, "ranges", overlayRes, append = TRUE, row.names = FALSE)


	# last Msg
	if(res) message(paste(length(rnames) , "ranges updated to database; Elapsed time:",
					round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins") )


	})

#' @rdname rangeMapProcess
#' @description Method 4: One shp file containing all ranges, ID is required.  Metadata are computed
setMethod("rangeMapProcess",
	signature = c(object = "rangeMapProcess",spdf = "SpatialPolygonsDataFrame", dir = "missing", ID = "character", metadata = "list", parallel = "logical"),
	definition = function(object, spdf, ID,  metadata, parallel){

	Startprocess = Sys.time()

	if(parallel) {
		library(doParallel)
		cl = makePSOCKcluster(detectCores())
		registerDoParallel(cl)
		on.exit( stopCluster(cl) )
		}

	message("Processsing ranges, please wait!...")

	cnv = as(canvasFetch(object), "SpatialPointsDataFrame")

	#  reproject
	p4s =  dbReadTable(object@CON, object@PROJ4STRING)[1,1]
	if(!identical(gsub(" ", "", proj4string(spdf)), gsub(" ", "", p4s) ) ) {
		warning( paste("Reprojecting to", dQuote(p4s)), keep = FALSE)
		spdf = spTransform( spdf , CRS(p4s) )
		}

	# split by range
	message( "Identifing ranges...")
	spdf = split(spdf, spdf@data[, ID])
	message( paste(length(spdf), " ranges found.") )

	rnames = names(spdf)

	message( "Processing ranges...")
	.processRange = function(x) {
			name = x@data[1, ID]
			pos = which(rnames%in%name)
			if(!parallel) setTxtProgressBar(pb, pos)
			rangeOverlay(x,  cnv, name)
		}

	if(!parallel) {
		pb = txtProgressBar(min = 0, max = length(rnames), char = ".", style = 3)
		overlayRes = lapply(spdf, .processRange)
		close(pb)
		}

	if(parallel)
		overlayRes = foreach(i = 1:length(spdf), .packages = 'rangeMapper') %dopar%  .processRange(spdf[[i]])

	overlayRes = do.call(rbind, overlayRes)

	names(overlayRes) = c(object@ID, object@BIOID)

	# save  to @RANGES
	message("Writing to project...")
	res = dbWriteTable(object@CON, "ranges", overlayRes, append = TRUE, row.names = FALSE)
	message(res)

	# last Msg
	if(res) message(paste(length(rnames) , "ranges updated to database; Elapsed time:",
						round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins") )


	# save  to @METADATA_RANGES
	message("Extracting metadata..")
	if(!parallel)
		rtr = lapply( spdf, function(R) sapply(metadata, function(x) x(R) ) )

	if(parallel)
		rtr = foreach(i = 1:length(spdf), .packages = 'rangeMapper') %dopar%  { sapply(metadata, function(x) x(spdf[[i]] ) )  }

	rtr = data.frame(do.call(rbind, rtr))

	  lapply(
		paste("ALTER TABLE metadata_ranges ADD COLUMN", names(rtr[, 1:ncol(rtr), drop = FALSE]), "FLOAT"),
			function(x)  dbGetQuery(object@CON, x))

	rtr = cbind(rownames(rtr), rtr)
	names(rtr)[1] = object@BIOID

	res = dbWriteTable(object@CON, object@METADATA_RANGES, rtr, append = TRUE, row.names = FALSE)
		message(res)

	})


#' Process ranges
#'
#' Each polygon range is overlayed on the canvas and the results are saved to
#' the active project file.
#'
#' The overlay is performed using \code{\link[sp]{overlay}}. If the overlay
#' returns no results (i.e. the polygon is smaller than a grid cell) then the
#' centroid of the range will snap to the nearest point and only one grid cell
#' will be returned for that range.
#'
#' @name processRanges
#' @param con An sqlite connection pointing to a valid \code{rangeMapper} project.
#' @param \dots see \code{\link[rangeMapper]{rangeMapProcess}}
#'
#' @note If thousands of individual range map polygons are processed, their
#' geometries are complex and/or the canvas resolution is relatively high this
#' step can be time consuming.
#' @seealso \code{\link[rangeMapper]{rangeMapper}}
#' \code{\link[rangeMapper]{rangeTraits}}
#' \code{\link[rangeMapper]{metadataUpdate}}.
#' @export
#' @keywords spatial
#' @examples
#'
#' require(rangeMapper)
#' wd = tempdir()
#'
#' \dontrun{
#' # Multiple files (one file per range)
#' rdr= system.file(package = "rangeMapper", "extdata", "wrens", "vector")
#' dbcon = rangeMap.start(file = "wrens.sqlite", overwrite = TRUE, dir = wd)
#' global.bbox.save(con = dbcon, bbox = rdr)
#' gridSize.save(dbcon, gridSize = 2) ; canvas.save(dbcon)
#' system.time(processRanges(dir = rdr, con =  dbcon))
#' }
#'
#' # One file containing all the ranges
#' r = readOGR(system.file(package = "rangeMapper", "extdata",
#' 	"wrens", "vector_combined"), "wrens", verbose = FALSE)
#'
#' dbcon = rangeMap.start(file = "wrens.sqlite", overwrite = TRUE, dir = wd )
#' global.bbox.save(con = dbcon, bbox = r)
#' gridSize.save(dbcon, gridSize = 2)
#' canvas.save(dbcon)
#'
#' system.time(processRanges(spdf = r, con =  dbcon, ID = "sci_name", parallel = FALSE))
#' # ~ 18 times faster than processing individual ranges.
#'
#'
processRanges <- function(con, ...) {
	x = new("rangeMapProcess", CON = con)
	rangeMapProcess(x, ... )
	}


