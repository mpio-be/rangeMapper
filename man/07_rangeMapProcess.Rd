\name{processRanges}
\alias{processRanges}
\alias{rangeMapProcess-methods}
\alias{rangeMapProcess}
\alias{rangeMapProcess,rangeMapProcess,missing,character,missing,list,missing-method}
\alias{rangeMapProcess,rangeMapProcess,missing,character,missing,missing,missing-method}
\alias{rangeMapProcess,rangeMapProcess,SpatialPolygonsDataFrame,missing,character,list,missing-method}
\alias{rangeMapProcess,rangeMapProcess,SpatialPolygonsDataFrame,missing,character,missing,missing-method}

\title{Process ranges}
\description{
Each polygon range is overlayed on the canvas and the results are saved to the active project file.
}

\details{The overlay is performed using \code{\link[sp]{overlay}}. If the overlay returns no results (i.e. the polygon is smaller than a grid cell) then the centroid of the range will snap to the nearest point and only one grid cell will be returned for that range.
}

\arguments{
	  \item{con}{An sqlite connection pointing to a valid \code{rangeMapper} project.}
	  	  \item{\dots}{Arguments to pass to the corresponding methods: \cr
				\emph{spdf} \code{\link[sp]{SpatialPolygonsDataFrame}} object containing all the ranges. \cr 
				\emph{ID} when spdf is set this is a \code{character} vector given the name of the range. \cr 
				\emph{dir} ranges file directory where the individual ranges shp files are located. In this case the range ID is the file name. \cr 
				\emph{metadata} a named list of functions. See \code{\link[rangeMapper]{rangeTraits}} and \code{\link[rangeMapper]{metadataUpdate}} \cr 
				\emph{parallel} for the moment no method defined for this signature. \cr 
		}
	  

	}

\usage{
processRanges(con,\dots)
}


 \note{
If thousands of individual range map polygons are processed, their geometries are complex and/or the canvas resolution is relatively high this step can be time consuming. 
 }


\seealso{
	\code{\link[rangeMapper]{rangeMapper}}
	\code{\link[rangeMapper]{rangeTraits}}
	\code{\link[rangeMapper]{metadataUpdate}}.
	}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\references{
Valcu, M., Dale, J. and Kempenaers, B. (2012)  rangeMapper: A platform for the study of macroecology of life history traits. 21(9). (DOI: 10.1111/j.1466-8238.2011.00739.x)
}


\examples{
require(rangeMapper)
wd = tempdir()

\dontrun{ 
# Multiple files (one file per range)
rdr= system.file(package = "rangeMapper", "extdata", "wrens", "vector")
dbcon = rangeMap.start(file = "wrens.sqlite", overwrite = TRUE, dir = wd)
global.bbox.save(con = dbcon, bbox = rdr)
gridSize.save(dbcon, gridSize = 2) ; canvas.save(dbcon) 
system.time(processRanges(dir = rdr, con =  dbcon))
}

# One file containing all the ranges
r = readOGR(system.file(package = "rangeMapper", "extdata", 
	"wrens", "vector_combined"), "wrens", verbose = FALSE)

dbcon = rangeMap.start(file = "wrens.sqlite", overwrite = TRUE, dir = wd )
global.bbox.save(con = dbcon, bbox = r)
gridSize.save(dbcon, gridSize = 2)  
canvas.save(dbcon) 

system.time(processRanges(spdf = r, con =  dbcon, ID = "sci_name" ))
# ~ 18 times faster than processing individual ranges.

}

 
\keyword{spatial}






















