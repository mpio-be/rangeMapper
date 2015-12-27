

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
#' breding_ranges = rgdal::readOGR(system.file(package = "rangeMapper",
#'      "extdata", "wrens", "vector_combined"), "wrens", verbose = FALSE)[1:10, ]
#' data(wrens)
#' d = subset(wrens, select = c('sci_name', 'body_size', 'body_mass', 'clutch_size') )
#' con = ramp("wrens.sqlite", gridSize = 10, spdf = breding_ranges, biotab = d, ID = "sci_name",
#'             metadata = rangeTraits(), FUN = "median", overwrite = TRUE)
#' all = rangeMap.fetch(con)
#' sr = rangeMap.fetch(con, 'species_richness')
#' plot(sr)
#' plot(all)
setMethod("plot", signature(x='SpatialPixelsRangeMap', y='missing'),
	function(x, colorpalette = brewer.pal.get('Spectral')[11:1], ncols = 20,
		    scales = FALSE, style = "equal",  ...) {

	colPal= colorRampPalette(colorpalette, space = "Lab")(ncols)

	mapVars = names(x)

	 nr <- nc <- ceiling(sqrt(length(mapVars )))

	layout = cbind(x = rep(1:nr[1], each = nc), y = rep(1:nr, nc), nr, nc)

	if(length(mapVars ) == 2)   layout[, 'nr'] = 1
	if(length(mapVars ) == 3)  layout = cbind(rep(1, 3), 1:3, 1, 3)

	for(i in seq(along = mapVars)) {
		Int = classIntervals(as.numeric(na.omit(x@data[,mapVars[i]])), n = ncols, style = style, ...)
		printMore = if(i<length(mapVars)) TRUE else FALSE

		print(spplot(x, mapVars[i] ,scales = list(draw = scales), cuts = ncols, checkEmptyRC = FALSE,
			  col.regions = colPal, at = Int$brks, main = if(length(mapVars) > 1) mapVars[i] else "", ...),
				split=layout[i, ], more=printMore)
		}

 	})

#' Plot a rmap.frame
#'
#' @param x       a a rmap.frame object.
#' @param colours a vector of colours to pass to \code{\link[ggplot2]{scale_fill_gradientn}}.
#' @param \dots   further arguments to pass to \code{\link[gridExtra]{arrangeGrob}}.
#' @return        a ggplot object.
#' @export
#' @examples
#' breding_ranges = rgdal::readOGR(system.file(package = "rangeMapper",
#'      "extdata", "wrens", "vector_combined"), "wrens", verbose = FALSE)
#' data(wrens)
#' d = subset(wrens, select = c('sci_name', 'body_mass', 'clutch_size') )
#' con = ramp("wrens.sqlite", gridSize = 1, spdf = breding_ranges, biotab = d, ID = "sci_name",
#'             FUN = "median", overwrite = TRUE)
#' m = rangeMap.fetch(con, c('median_body_mass', 'median_clutch_size'), spatial = FALSE)
#' plot(m, ncol = 2)


setMethod("plot", signature(x='rmap.frame', y='missing'),
		function(x, colours = palette_rangemap('set1'), ... ) {
	idv = setdiff(names(x), c('x', 'y') )

	out = lapply(idv, function (v) {
		ggplot(data = x[!is.na(eval(parse(text=v)))]) +
			geom_tile( aes_string(x = 'x', y = 'y', fill = v ) ) +
			coord_equal() +
			scale_fill_gradientn(colours = colours, name = '') +
			labs(x=NULL, y=NULL) +
			ggtitle(v) +
			theme_rangemap()
		})

	gg = lapply(out, ggplotGrob)

	if( length(idv) == 1) out[[1]]  else
	grid.arrange(  arrangeGrob(grobs = gg, ...)  )


 	})


