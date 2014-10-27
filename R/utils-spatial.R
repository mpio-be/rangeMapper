

WKT2SpatialPolygonsDataFrame <- function(dat, geom, id) {	
	dl = split(dat, dat[, id])	
	
	o = lapply(dl, function(x) { 
		
		p = mapply(readWKT, text = x[, geom], id = 1:nrow(x), USE.NAMES = FALSE )
			if(length(p) == 1) {
				p = p[[1]]
				p = spChFIDs(p, as.character(x[1, id]))
				}

			if(length(p) > 1) {
				p = do.call(rbind, p)
				p = gUnionCascaded(p, id = as.character(x[1, id]) )
				}
	p
	})

	X = do.call(rbind, o)
	dat = data.frame(id = sapply(slot(X, "polygons"), function(x) slot(x, "ID")) )
	row.names(dat ) = dat$id
	names(dat) = id
	X = SpatialPolygonsDataFrame(X, data =  dat)
	X

	}

	

