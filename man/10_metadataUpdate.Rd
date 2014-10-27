\name{metadata.update}
\alias{metadata.update}
\alias{metadataUpdate}
\alias{metadataUpdate-methods}
\alias{metadataUpdate,rangeMap,function,character,SpatialGridDataFrame,missing-method}
\alias{metadataUpdate,rangeMap,function,character,SpatialGridDataFrame,missing-method}



\title{Updates metadata table}

\description{
	Updates \code{metadata_table} of a \code{rangeMapper} project \emph{after} importing ranges with \code{\link{processRanges}}.
	}

\usage{
metadata.update(rangeMap, FUN, name, map, overwrite = FALSE, ...)
}


\arguments{
	  \item{rangeMap}{A \code{\link{rangeMap}} object.}
	  \item{FUN}{Function used to aggregate the map values corresponding to each range}
	  \item{name}{The name of the new \code{metadata_table} field containing the variable computed by \code{FUN}}
	  \item{map}{Single-band \code{\link[sp]{SpatialGridDataFrame}} object}
	  \item{overwrite}{If set to \code{TRUE} the the values of the field are replaced}
	  \item{\dots}{extra arguments (e.g. \code{na.rm = TRUE}) to be passed to FUN.}

}  

\value{
Nothing is returned.
}


\note{
In order to compute taxa-level metadata which are not dependent on the project's resolution use 
\code{\link{processRanges}} with a \code{metadata} argument. See \code{\link{rangeTraits}} for more details. \cr
The method can be extended to work with raster or vector objects (e.g. lines, polygons, points)
using overlaying functions in the package \code{raster} and \code{rgeos} respectively. 
}


\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\seealso{
	\code{\link{processRanges}} \cr
	\code{\link{rangeTraits}}

	}
	
\references{
Valcu, M., Dale, J. and Kempenaers, B. (2012)  rangeMapper: A platform for the study of macroecology of life history traits. 21(9). (DOI: 10.1111/j.1466-8238.2011.00739.x)
}
	
\examples{

require(rangeMapper)

# data
spdf = readOGR(system.file(package = "rangeMapper", 
	"extdata", "wrens", "vector_combined"), "wrens", verbose = FALSE)
rloc = system.file(package = "rangeMapper", "extdata",
	"etopo1", "etopo1_Americas.tif")
r = readGDAL(rloc, output.dim = c(50, 50))
spdf = spTransform(spdf, CRS(proj4string(r)) )

# the project 
dbcon = rangeMap.start(file = "wrens.sqlite", overwrite = TRUE, 
	dir = tempdir() )
rmap = new("rangeMap", CON = dbcon)	
global.bbox.save(con = dbcon, bbox = spdf )
gridSize.save(dbcon, gridSize = 300000)  
canvas.save(dbcon) 
processRanges(spdf = spdf, con =  dbcon, ID = "sci_name" )

# metadata.update
metadata.update (rmap,
			FUN = function(x, ...) {
				res = diff(range(x, ...))
				if( !is.finite(res)) res = 0
				res 
				},
	name = 'AltitudeRange', map = r, na.rm = TRUE, overwrite = TRUE)

# plot
mr = dbGetQuery(dbcon, 'select * from metadata_ranges')
maxRangeSp = mr[mr$AltitudeRange== max(mr$AltitudeRange), 'bioid']
image(r)
plot(rangeFetch(rmap, maxRangeSp), add = TRUE, border = 4, lwd = 3)
title(main = maxRangeSp)



}

\keyword{spatial}
\keyword{sqlite}





















