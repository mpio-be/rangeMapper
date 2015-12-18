
if (!isGeneric("plot"))    {
	setGeneric("plot", function(x,y,...) standardGeneric("plot"))
	}


#' Plot a SpatialPixelsRangeMap
#'
#' This is a wrapper around \code{\link{sp::spplot}}
#'
#' @name plot
#' @aliases SpatialPixelsRangeMap, brewer.pal.get
#' @param palette The name of a color palette. When NULL all the qualitative
#' color palettes are returned.
#' @note \code{brewer.pal.get} is a simple wrapper around \code{\link{RColorBrewer::brewer.pal.info}}
#' @section Methods:\describe{\item{list("signature(x =\"SpatialPixelsRangeMap\", y = \"missing\")")}{
#'
#' \bold{x=SpatialPixelsRangeMap}
#'
#' \code{plot(x, colorpalette = RColorBrewer::brewer.pal.get('Spectral')[11:1], ncols = 20,
#' scales = FALSE, style = "equal", \dots{})}
#'
#' \tabular{rl}{
#'   \code{x}              \tab a SpatialPixelsRangeMap object.\cr
#'    \code{colorpalette} \tab A color palette. \cr
#'    \code{ncols}        \tab Number of color classes required, default to 20; argument to be passed to
#'    \code{\link{classInt::classIntervals}}\cr
#'    \code{scales}       \tab If \sQuote{FALSE}, default, axes scale are not drawn.\cr
#'    \code{style}        \tab class interval style; see \code{\link{classInt::classIntervals}} for more details\cr
#'    \code{\dots{} }     \tab Any argument that can be passed to \code{\link{spplot}}\cr }
#' }}
#'
#'
#' @seealso \code{\link{classInt::classIntervals}} \code{\link{RColorBrewer::brewer.pal}}
#' @export
#' @keywords methods
#' @examples
#'
#' require(rangeMapper)
#' dbcon = rangeMap.start(file = "test.sqlite", overwrite = TRUE, dir = tempdir() )
#' f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
#' global.bbox.save(con = dbcon, bbox = f,
#' 	p4s = CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=km +no_defs") )
#' gridSize.save(dbcon, gridSize = 200)  # cell size 2 deg
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
#' pal =  brewer.pal(11, 'RdYlGn')[11:1]
#'
#' plot(SR, style = "fisher", colorpalette = pal)
#'
setMethod("plot",
	signature(x='SpatialPixelsRangeMap', y='missing'),
	function(x, colorpalette = brewer.pal.get('Spectral')[11:1],
		ncols = 20, scales = FALSE, style = "equal",  ...) {
	  # ...arguments to pass to classInt::classIntervals (e.g. n) and spplot

	colPal= colorRampPalette(colorpalette, space = "Lab")(ncols)

	mapVars = names(x)

	 nr <- nc <- ceiling(sqrt(length(mapVars )))

	layout = cbind(x = rep(1:nr[1], each = nc), y = rep(1:nr, nc), nr, nc)

	if(length(mapVars ) == 2)   layout[, 'nr'] = 1
	if(length(mapVars ) == 3)  layout = cbind(rep(1, 3), 1:3, 1, 3)

	for(i in seq(along = mapVars)) {

	Int = classInt::classIntervals(as.numeric(na.omit(x@data[,mapVars[i]])), n = ncols, style = style, ...)
	printMore = if(i<length(mapVars)) TRUE else FALSE

	print(spplot(x, mapVars[i] ,scales = list(draw = scales), cuts = ncols, checkEmptyRC = FALSE, col.regions = colPal,
		 at = Int$brks, main = if(length(mapVars) > 1) mapVars[i] else "", ...),
			split=layout[i, ], more=printMore)


	}})

# TODO
ggp <- function(){

		g = ggplot(data = m) + geom_tile( aes(x, y, fill = species_richness) ) +
		coord_equal() +
		scale_fill_gradientn(
				colours = RColorBrewer::brewer.pal(5, "YlGnBu"),
				guide = guide_legend(title = "xxxxxx", keywidth = 1, keyheight = .5) ) +
		theme_bw() +
		labs(x=NULL, y=NULL) +
		theme(axis.text = element_blank(), axis.ticks = element_blank() ,
			 legend.justification=c(0,0), legend.position=c(0,0),
			 panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank() )


	}


