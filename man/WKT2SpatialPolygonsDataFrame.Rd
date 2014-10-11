\name{WKT2SpatialPolygonsDataFrame}
\alias{WKT2SpatialPolygonsDataFrame}

\title{
Convert WKT polygons to SpatialPolygonsDataFrame
}
\description{
Convert a data.frame containing WKT polygons to a SpatialPolygonsDataFrame.
  }

\usage{
  WKT2SpatialPolygonsDataFrame(dat, geom, id)
  }

\arguments{
  \item{dat}{ \code{data.frame}
}
  \item{geom}{
  is the name (character vector) of the column in the \code{data.frame} containing the geometry.
}
  \item{id}{
  is the name (character vector) of the column in the \code{data.frame} identifying the polygon.
 when \code{id} is not unique then polygons are combined using \code{rgeos::gUnionCascaded}.
}

}

\value{
 a \code{SpatialPolygonsDataFrame} object.
}

\examples{
require(rangeMapper)
require(rgeos)

# generate a few random polygons
randPoly = function(mean, sd) {
  writeWKT(
    gConvexHull(
      readWKT(paste("MULTIPOINT (", 
             paste(apply(matrix(rnorm(n= 100, mean, sd), ncol = 2), 1, 
             paste, collapse = ' '), collapse = ","), ")")))) 
}
n = 50
d = data.frame( nam = sample(letters, n, TRUE), 
               range = mapply(randPoly, mean = sample(1:2, n, TRUE), 
               sd = sample(1:2/5, n, TRUE) ))

  
X = WKT2SpatialPolygonsDataFrame(d, 'range', 'nam')


dbcon = rangeMap.start(file = "test.sqlite", overwrite = TRUE, dir = tempdir() )
global.bbox.save(con = dbcon, bbox = X) 
gridSize.save(dbcon)
canvas.save(dbcon)
processRanges(spdf = X, con =  dbcon, ID = "nam" )
rangeMap.save(dbcon)
plot(rangeMap.fetch(dbcon))

}


\keyword{spatial}

