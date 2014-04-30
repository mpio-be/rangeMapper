

	WKT2SpatialPolygonsDataFrame <- function(dat, geom, id) {	
	
		dl = split(d, d[, id])	
		o = lapply(dl, function(x) { 
				p = mapply(readWKT, text = x[, geom], id = 1:nrow(x)	 )
				p = do.call(rbind, p)
				gUnionCascaded(p, id = as.character(x[1, id]) )
			})
		X = do.call(rbind, o)
		dat = data.frame(id = sapply(slot(X, "polygons"), function(x) slot(x, "ID")) )
		row.names(dat ) = dat$id
		SpatialPolygonsDataFrame(X, data =  dat)
	
	}
	
	
	
	# move to .Rd
	
	require(rangeMapper)
	require(rgeos)

	# EXAMPLE
	randPoly = function(mean, sd) {
	 writeWKT(
	 gConvexHull(
		readWKT(paste("MULTIPOINT (", 
			paste(apply(matrix(rnorm(n= 100, mean, sd), ncol = 2), 1, 
			 paste, collapse = ' '), collapse = ","), ")")))) 
	}
	n = 10
	d = data.frame( id = sample(letters, n, TRUE), 
		range = mapply(randPoly, mean = sample(1:2, n, TRUE), 
		sd = sample(1:2/5, n, TRUE) ))

	# FUN	
	WKT2SpatialPolygonsDataFrame <- function(dat, geom, id) {	
	
		dl = split(d, d[, id])	
		o = lapply(dl, function(x) { 
				p = mapply(readWKT, text = x[, geom], id = 1:nrow(x)	 )
				p = do.call(rbind, p)
				gUnionCascaded(p, id = as.character(x[1, id]) )
			})
		X = do.call(rbind, o)
		dat = data.frame(id = sapply(slot(X, "polygons"), function(x) slot(x, "ID")) )
		row.names(dat ) = dat$id
		SpatialPolygonsDataFrame(X, data =  dat)
	
	}
	
	# rangeMapper
	WKT2SpatialPolygonsDataFrame(d, 'range', 'id')
	
	
	dbcon = rangeMap.start(file = "test.sqlite", overwrite = TRUE, dir = tempdir() )
	global.bbox.save(con = dbcon, bbox = X) 
	gridSize.save(dbcon)
	canvas.save(dbcon)
	processRanges(spdf = X, con =  dbcon, ID = "id" )
	rangeMap.save(dbcon)
	plot(rangeMap.fetch(dbcon))