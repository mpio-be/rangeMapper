
setGeneric("vertices", function(object, FUN)  standardGeneric("vertices") )

setMethod("vertices", "SpatialPolygonsDataFrame", 
	function(object, FUN) {
		d = lapply( unlist(lapply(slot(object, "polygons"), function(P) slot(P, "Polygons"))), function(cr) slot(cr, "coords") )
		d = lapply(d, function(x) { dimnames(x)[[2]] = c('x', 'y'); x} )
		d = lapply(d, function(x) x[-nrow(x), , drop = FALSE])
		d = mapply("cbind", 1:length(d), d, SIMPLIFY = FALSE)
		if(!missing(FUN))
			d = lapply(d, function(x) apply(x ,2, FUN) )

		d = data.frame(do.call("rbind", d))

		coordinates(d) = ~ x+y
		proj4string(d) = CRS(proj4string(object))
		names(d) = "id"
		d
	})

rangeTraits <- function(..., use.default = TRUE) {

	Area     = function(spdf) sum(sapply(slot(spdf, "polygons"), function(x) slot(x, "area") ))
	Median_x = function(spdf) median(coordinates(vertices(spdf))[, 1])
	Median_y = function(spdf) median(coordinates(vertices(spdf))[, 2])
	Min_x    = function(spdf) min(coordinates(vertices(spdf))[, 1])
	Min_y    = function(spdf) min(coordinates(vertices(spdf))[, 2])
	Max_x    = function(spdf) max(coordinates(vertices(spdf))[, 1])
	Max_y    = function(spdf) max(coordinates(vertices(spdf))[, 2])

	
	res = list(Area = Area, Median_x = Median_x, Median_y = Median_y, Min_x = Min_x, Min_y = Min_y, Max_x = Max_x, Max_y = Max_y)
	
	x = list(...)
	if(length(x) > 0) {
		 if(length(names(x)) != length(x)) stop (dQuote("..."), " elements should be named, e.g. myFun = abc")
		 if( !all(sapply(x, is.function))) stop (dQuote("..."), " elements should be functions.")
		 if(use.default) res = c(res, x)
	}
	res
	}

.rangeOverlay <- function(spdf, canvas, name) {
	#SpatialPolygonsDataFrame
	# canvas 	SpatialPointsDataFrame
	# name character, length 2
	
	overlayRes = which(!is.na(over(canvas, spdf)[, 1])) 
	
	if(length(overlayRes) > 0) { 	# do grid interpolation
		sp = canvas[overlayRes, ]
		o = data.frame(id = sp$id, bioid = rep(name, nrow(sp)) ) 
		}
		
	if(length(overlayRes) == 0) { 	# the polygons are smaller than the grid cells:  snap to the nearest points
			xy = vertices(spdf, FUN = mean)
			nn = spDists(canvas, xy)
			mins = apply(nn, 2, min)
			res = vector(mode = 'numeric', length = length(mins))
			for(i in 1:length(res)) {
				res[i] = which(nn[,i] == mins[i])
				}
			res = unique(res)
			
			sp = canvas[res, ]
			
			o = data.frame(id = sp$id, bioid = rep(name, nrow(sp@coords)) )
		} 
	
	return(o)

	}

setGeneric("rangeMapProcess", function(object,spdf, dir, ID,metadata, parallel)  standardGeneric("rangeMapProcess") )

	# Method 1.1 :  Each range file is a separate shp file. No metadata
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

		o = .rangeOverlay(r,  cnv, name) 

		names(o) = c(object@ID, object@BIOID)

		# save  to db
		dbWriteTable(object@CON, object@RANGES, o, append = TRUE, row.names = FALSE) 

		}		

		lapply (1:length(Files$layer), FUN = .processRange) 


		# last Msg
		message(paste(nrow(Files), "ranges updated to database; Elapsed time:", 
						round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins") )

		} 
		)

	#  Method 1.2 :  Each range file is a separate shp file. Metadata are computed
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


		o = .rangeOverlay(r,  cnv, name) 

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

		# Method 2.1:  One shp file containing all ranges, ID is required. No metadata
		setMethod("rangeMapProcess",  
		signature = c(object = "rangeMapProcess",spdf = "SpatialPolygonsDataFrame", dir = "missing", ID = "character", metadata = "missing", parallel = "missing"), 
		definition = function(object, spdf, ID,  metadata){

		Startprocess = Sys.time()

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
		pb = txtProgressBar(min = 0, max = length(rnames), char = ".", style = 3)

		message( "Processing ranges...")
		.processRange = function(x) {
			name = x@data[1, ID]
			pos = which(rnames%in%name)
			setTxtProgressBar(pb, pos)
			.rangeOverlay(x,  cnv, name) 
		}

		overlayRes = lapply(spdf, .processRange)

		close(pb)

		overlayRes = do.call(rbind, overlayRes)	

		names(overlayRes) = c(object@ID, object@BIOID) 

		message("Writing to project.")		
		res = dbWriteTable(object@CON, "ranges", overlayRes, append = TRUE, row.names = FALSE) 


		# last Msg
		if(res) message(paste(length(rnames) , "ranges updated to database; Elapsed time:", 
						round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins") )


		})	
	
	# Method 2.2:  One shp file containing all ranges, ID is required.  Metadata are computed
	setMethod("rangeMapProcess",  
		signature = c(object = "rangeMapProcess",spdf = "SpatialPolygonsDataFrame", dir = "missing", ID = "character", metadata = "list", parallel = "missing"), 
		definition = function(object, spdf, ID,  metadata){

		Startprocess = Sys.time()

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
		pb = txtProgressBar(min = 0, max = length(rnames), char = ".", style = 3)

		message( "Processing ranges...")
		.processRange = function(x) {
				name = x@data[1, ID]
				pos = which(rnames%in%name)
				setTxtProgressBar(pb, pos)
				.rangeOverlay(x,  cnv, name) 
			}

		overlayRes = lapply(spdf, .processRange)
			
		close(pb)

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
		rtr = lapply( spdf, function(R) sapply(metadata, function(x) x(R) ) )
		rtr = data.frame(do.call(rbind, rtr))

		  lapply( 
			paste("ALTER TABLE metadata_ranges ADD COLUMN", names(rtr[, 1:ncol(rtr), drop = FALSE]), "FLOAT"), 
				function(x)  dbGetQuery(object@CON, x))

		rtr = cbind(rownames(rtr), rtr)
		names(rtr)[1] = object@BIOID

		res = dbWriteTable(object@CON, object@METADATA_RANGES, rtr, append = TRUE, row.names = FALSE) 
			message(res)

	})	


# user level function
processRanges <- function(con, ...) {
	x = new("rangeMapProcess", CON = con)
	rangeMapProcess(x, ... )	
	}


















