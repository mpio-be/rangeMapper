

if (!isGeneric("plot"))    { 
	setGeneric("plot", function(x,y,...) standardGeneric("plot")) 
	}


# ...arguments to pass to classIntervals (eg. n) and spplot

setMethod("plot", signature(x='SpatialPixelsRangeMap', y='missing'), 

function(x, colorpalette = brewer.pal.get('Spectral')[11:1], ncols = 20, scales = FALSE, style = "equal",  ...) {

		trellis.par.set("regions", list(col= colorRampPalette(colorpalette, space = "Lab")(ncols) ) , warn = FALSE)
		
		mapVars = names(x)
		
		 nr <- nc <- ceiling(sqrt(length(mapVars )))

		layout = cbind(x = rep(1:nr[1], each = nc), y = rep(1:nr, nc), nr, nc)	
		
		if(length(mapVars ) == 2)   layout[, 'nr'] = 1
		if(length(mapVars ) == 3)  layout = cbind(rep(1, 3), 1:3, 1, 3)

		for(i in seq(along = mapVars)) {
		
		Int = classIntervals(as.numeric(na.omit(x@data[,mapVars[i]])), n = ncols, style = style, ...)
		printMore = if(i<length(mapVars)) TRUE else FALSE
		
		print(spplot(x, mapVars[i] ,scales = list(draw = scales), cuts = ncols, checkEmptyRC = FALSE, 
			 at = Int$brks, main = if(length(mapVars) > 1) mapVars[i] else "", ...), 
				split=layout[i, ], more=printMore)
		}
	}	
)	





