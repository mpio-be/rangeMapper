
\name{plot-methods}
\docType{methods}
\alias{plot,SpatialPixelsRangeMap,missing-method}
\alias{brewer.pal.get}

\title{Plot a SpatialPixelsRangeMap}

\description{
This is a wrapper around \code{\link{spplot}}
}

\section{Methods}{
	\describe{
	\item{\code{signature(x = "SpatialPixelsRangeMap", y = "missing")}}{

	\bold{x=SpatialPixelsRangeMap}

	\code{plot(x, colorpalette = brewer.pal.get('Spectral')[11:1], ncols = 20, scales = FALSE, style = "equal",  \dots)}

	\tabular{rl}{
	\code{x} \tab a SpatialPixelsRangeMap object.\cr
	\code{colorpalette}  \tab  A color palette. \cr 
	\code{ncols}  \tab  Number of color classes required, default to 20; argument to be passed to \code{\link{classIntervals}}\cr   
	\code{scales}  \tab  If \sQuote{FALSE}, default, axes scale are not drawn.\cr 
	\code{style}  \tab  class interval style; see \code{\link{classIntervals}} for more details\cr  
	\code{\dots}  \tab  Any argument that can be passed to \code{\link{spplot}}\cr
	}
	}}}

\arguments{
  \item{palette}{The name of a color palette. When NULL all the qualitative color palettes are returned.}
}

\note{
\code{brewer.pal.get} is a simple wrapper around \code{\link{brewer.pal.info}}
}
	
\examples{
require(rangeMapper)
dbcon = rangeMap.start(file = "test.sqlite", overwrite = TRUE, dir = tempdir() )
f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
global.bbox.save(con = dbcon, bbox = f, 
	p4s = CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=km +no_defs") ) 
gridSize.save(dbcon, gridSize = 200)  # cell size 2 deg
canvas.save(dbcon) 
processRanges(spdf = readOGR(f, "wrens", verbose = FALSE), con =  dbcon, ID = "sci_name")
rangeMap.save(dbcon) # species richness


# PLOTS
all = rangeMap.fetch(dbcon)
SR = rangeMap.fetch(dbcon, 'species_richness')

plot(all)
plot(SR, style = "fisher", sub = "Wrens species richness")

pal = brewer.pal.get('RdYlGn')[11:1]

plot(SR, style = "fisher", colorpalette = pal)

	
}


	

\seealso{
  \code{\link{classIntervals}}
  \code{\link{brewer.pal}}

  
}


\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\keyword{methods}





	




















