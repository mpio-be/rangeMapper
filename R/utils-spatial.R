

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
	
	

