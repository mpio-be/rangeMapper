\name{rangeFetch}
\alias{rangeFetch}

\title{Range extractor}
\description{
Fetch an arbitrary range from a rangeMapper project. 
}


\arguments{
	  \item{rangeMap}{A \code{\link{rangeMap}} object.}
	  \item{bioid}{A character vector, usually a taxon name, which identifies a range within a given rangeMapper project.}
	}

\usage{
rangeFetch(rangeMap, bioid)
}

\value{
A \code{\link{SpatialPolygons}}. 
}


\seealso{
	\code{\link[rangeMapper]{rangeMapper}}.
	\code{\link[rangeMapper]{rangeMapFetch}}.
	\code{\link[rangeMapper]{rangeMapSave}}.

	}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\references{
Valcu, M., Dale, J. and Kempenaers, B. (2012)  rangeMapper: A platform for the study of macroecology of life history traits. 21(9). (DOI: 10.1111/j.1466-8238.2011.00739.x)
}


\examples{
wd = setwd(tempdir())
require(rangeMapper)
spdf = readOGR(system.file(package = "rangeMapper", "extdata", 
	"wrens", "vector_combined"), "wrens", verbose = FALSE)
dbcon = rangeMap.start(file = "wrens.sqlite", 
	overwrite = TRUE, dir = tempdir() )
rmo = rangeMap("wrens.sqlite")
global.bbox.save(con = dbcon, bbox = spdf)
gridSize.save(dbcon, gridSize = 3)  
canvas.save(dbcon) 
processRanges(spdf = spdf, con =  dbcon, ID = "sci_name" )
rangeMap.save(dbcon)

house_wren = rangeFetch(rmo, "Troglodytes_aedon")
image(rangeMap.fetch(dbcon))
plot(house_wren, add = TRUE, border = 'blue', lwd = 2)
setwd(wd)




}

 
\keyword{spatial}






















