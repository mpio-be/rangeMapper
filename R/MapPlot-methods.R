

#' Plot a SpatialPixelsRangeMap
#'
#' This is a wrapper around \code{spplot}
#'
#' @param x             A SpatialPixelsRangeMap
#' @param colorpalette  A color palette.
#' @param ncols         Number of color classes required, default to 20; argument to be passed to
#'                      \code{\link[classInt]{classIntervals}}
#' @param scales        If \sQuote{FALSE}, default, axes scale are not drawn.
#' @param style         Class interval style; see \code{\link[classInt]{classIntervals}} for more details
#' @param \dots         Any argument that can be passed to see \code{\link[sp]{spplot}}
#'
#' @export
#' @examples
#' require(rangeMapper)
#' require(rgdal)
#' dbcon = rangeMap.start(file = "test.sqlite", overwrite = TRUE, dir = tempdir() )
#' f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
#' global.bbox.save(con = dbcon, bbox = f,
#' p4s = CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=km +no_defs") )
#' gridSize.save(dbcon, gridSize = 500)  # cell size 2 deg
#' canvas.save(dbcon)
#' processRanges(spdf = readOGR(f, "wrens", verbose = FALSE), con =  dbcon, ID = "sci_name")
#' rangeMap.save(dbcon) # species richness
#'
#'
#' # PLOTS
#' all = rangeMap.fetch(dbcon)
#' SR = rangeMap.fetch(dbcon, 'species_richness')
#'
#' plot(all)
#' plot(SR, style = "fisher", sub = "Wrens species richness")
#'
#' pal =  RColorBrewer::brewer.pal(11, 'RdYlGn')[11:1]
#'
#' plot(SR, style = "fisher", colorpalette = pal)
#'
setMethod("plot", signature(x='SpatialPixelsRangeMap', y='missing'),
	function(x, colorpalette = brewer.pal.get('Spectral')[11:1],
		     ncols = 20, scales = FALSE, style = "equal",  ...) {

	colPal= colorRampPalette(colorpalette, space = "Lab")(ncols)

	mapVars = names(x)

	 nr <- nc <- ceiling(sqrt(length(mapVars )))

	layout = cbind(x = rep(1:nr[1], each = nc), y = rep(1:nr, nc), nr, nc)

	if(length(mapVars ) == 2)   layout[, 'nr'] = 1
	if(length(mapVars ) == 3)  layout = cbind(rep(1, 3), 1:3, 1, 3)

	for(i in seq(along = mapVars)) {
		Int = classIntervals(as.numeric(na.omit(x@data[,mapVars[i]])), n = ncols, style = style, ...)
		printMore = if(i<length(mapVars)) TRUE else FALSE

		print(spplot(x, mapVars[i] ,scales = list(draw = scales), cuts = ncols, checkEmptyRC = FALSE, col.regions = colPal,
			 at = Int$brks, main = if(length(mapVars) > 1) mapVars[i] else "", ...),
				split=layout[i, ], more=printMore)
		}

 	})

#' Plot a rmap.frame
#'
#' @param x a a rmap.frame object.
#' @return  a ggplot object.
#' @export

setMethod("plot", signature(x='rmap.frame', y='missing'), function(x) {
	idv = setdiff(names(x), c('x', 'y') )

	xl = melt(x, id.vars = c('x', 'y') ,   measure.vars = idv)
	xl = xl[!is.na(value)]

	ggplot(data = xl) +
		geom_tile( aes_string(x = 'x', y = 'y', fill = 'value') ) +
		{if(length(idv) > 1) facet_grid(~variable, scales = 'free')}  +
		coord_equal() +
		scale_fill_gradientn(colours = RColorBrewer::brewer.pal(5, "YlGnBu") ) +
		labs(x=NULL, y=NULL) +
		theme_rangemap()

 	})


