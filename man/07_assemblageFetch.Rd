\name{assemblageFetch-methods}
\docType{methods}
\alias{assemblageFetch-methods}
\alias{assemblageFetch}
\alias{assemblageFetch,rangeMap,SpatialPoints,character-method}
\alias{assemblageFetch,rangeMap,SpatialPoints,missing-method}
\title{ Function \code{assemblageFetch}  retrieves the species set of an arbitrary canvas cell}
\description{
 Function \code{assemblageFetch}  retrieves the species set, and the data associated with it, of an arbitrary canvas cell optionally with the associated life history data}  

\section{Methods}{
\describe{

\item{\code{signature(object = "rangeMap", xy = "SpatialPoints", BIO = "character")}}{
assemblageFetch(rangeMap, xy, BIO) returns a data.frame containing the bioid (e.g. species names), canvas id and any associated life history data contained in the BIO table.
}

\item{\code{signature(object = "rangeMap", xy = "SpatialPoints", BIO = "missing")}}{
If \code{BIO} is missing then assemblageFetch(rangeMap, xy) returns only a data.frame containing two columns the bioid (e.g. species names) and the canvas id.
}
}}
\keyword{methods}
\keyword{spatial}

\examples{
require(rangeMapper)

projName = "wrens.sqlite"
projLoc = paste(tempdir(), projName, sep = .Platform$file.sep)

dbcon = rangeMap.start(file = projName,dir = tempdir() , overwrite = TRUE)
f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
global.bbox.save(con = dbcon, bbox = f) 
gridSize.save(dbcon, gridSize = 3) 
canvas.save(dbcon) 
data(wrens)
bio.save(con = dbcon, loc = wrens ,  ID = "sci_name")
r = readOGR(f, "wrens", verbose = FALSE)
processRanges(spdf = r, con =  dbcon, ID = "sci_name")
rangeMap.save(dbcon)

sr = rangeMap.fetch(dbcon)
image(sr, axes = TRUE); grid()

p = list(x = -76.39, y = 9.26)
# or use locator:  p =  locator(1) 

xy = SpatialPoints( do.call(cbind, p), proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs ") )
af = assemblageFetch(rangeMap(projLoc) , xy)
points(p, col = 4, cex = 2)
print(af)

af = assemblageFetch(rangeMap(projLoc) , xy, "wrens")
print(af[, c(1, 4, 6:8)])



}
