\name{vertices-methods}
\docType{methods}
\alias{vertices}
\alias{vertices-methods}
\alias{vertices,SpatialPolygonsDataFrame-method}
\alias{vertices.SpatialPolygonsDataFrame}

 \title{Vertices of a SpatialPolygonsDataFrame}
\description{
Extract vertices from a \link[sp]{SpatialPolygonsDataFrame} and optionally applies an aggregating function to each Polygon. 
}


\section{Methods}{\describe{
	\item{"SpatialPolygonsDataFrame", FUN = function}{Extract Polygon vertices and remove the last (repeated line) of each Polygon.}

	}}


\value{
A \link[sp]{SpatialPointsDataFrame} containing an id column corresponding to each extracted Polygon.
}	  	
	


\seealso{
	\code{\link[sp]{coordinates}}.
	}

\author{ Mihai Valcu \email{valcu@orn.mpg.de} }

\references{
Valcu, M., Dale, J. and Kempenaers, B. (2012)  rangeMapper: A platform for the study of macroecology of life history traits. 21(9). (DOI: 10.1111/j.1466-8238.2011.00739.x)
}


\examples{
require(rangeMapper)
f = system.file(package = "rangeMapper", "extdata", "wrens", "vector")
# path to Campylorhynchus_gularis breeding range:
camgul = selectShpFiles(f, ogr = TRUE, polygons.only = TRUE)[6, ] 
r = readOGR(camgul$dsn, camgul$layer)
mp = vertices(r, mean)
v = vertices(r)

plot(r)
points(mp, col = 2, pch = 3, cex = 2)
points(v, pch = 3, cex = .5)
}

 
\keyword{spatial}
















