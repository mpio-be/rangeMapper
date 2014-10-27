
\name{rangeMap.save}
\alias{rangeMap.save}
\alias{rangeMapSave}
\alias{rangeMapSave-methods}
\alias{rangeMapSave,rangeMapSave,character,missing-method}
\alias{rangeMapSave,rangeMapSave,function,formula-method}
\alias{rangeMapSave,rangeMapSave,function,missing-method}
\alias{rangeMapSave,rangeMapSave,missing,missing-method}
\alias{rangeMapFetch}
\alias{rangeMapFetch-methods}
\alias{rangeMapFetch,rangeMapFetch-method}
\alias{rangeMap.fetch}



\title{Save, retrieve and export maps.}
\description{
Apply a chosen \code{SQL} or \R function at each grid cell, allowing for complex subsetting at both ID (e.g. species) and pixel (e..g assemblage) levels.}

\arguments{
	  \item{CON}{An sqlite connection pointing to a valid \code{rangeMapper} project. }
	  \item{tableName}{Name of the table (quoted) to be added to the sqlite database. The prefix \sQuote{MAP} will be appended to \code{tableName} prior to saving.}
	  \item{FUN}{the function to be applied to each pixel. If \code{FUN} is missing then species richness (species count) is computed.}
	  \item{biotab}{character string identifying the \sQuote{BIO} table to use.}
	  \item{biotrait}{character string identifying the ID of the \sQuote{BIO} table. see \code{\link{bio.save}} }
	 \item{subset}{A named \code{\link{list}}. See details} 
	  \item{path}{Path to the raster file(quoted) to be imported to the existing project. \code{raster package} is required at this step.}			
	  \item{overwrite}{If \code{TRUE} then the table is removed}			
		
	\item{\dots}{When \code{FUN} is an \R function, \dots denotes any extra arguments to be passed to it.}
}


\usage{
rangeMap.save(CON, tableName, FUN, biotab, biotrait, subset, path , overwrite, \dots)
}
	

\value{
\code{TRUE} when the MAP was created successfully. 
\code{rangeMap.fetch} returns a \code{\link{SpatialPixelsRangeMap-class}}. 

}	

\details{
The subset argument accepts a named list. Names refers to \sQuote{BIO}, \sQuote{MAP} and \sQuote{metadata_rages} table names 
while the strings in the list are character strings containing the SQL  \code{WHERE} clause.
The subset can point to either one table type 
(e.g. \code{list(MAP_species_richness = "species_richness > 500")} )
or can point to several table types 
(e.g. \code{list(BIO_lifeHistory = "clutch_size > 4", MAP_meanAltitude = "meanAltitude < 1000",  metadata_ranges = "Area < 1000")} )

Any valid SQL expression can be used to build up a subset. See \url{http://www.sqlite.org/lang_expr.html}
}


\note{
\code{SQL} aggregate functions are more efficient then their \R counterparts. For simple aggregate functions like mean, median, sd, count it is advisable to use \code{SQL} functions rather then R functions. 

}



\seealso{
\code{\link[rangeMapper]{metadataUpdate}}.
}


\references{
Valcu, M., Dale, J. and Kempenaers, B. (2012)  rangeMapper: A platform for the study of macroecology of life history traits. 
21(9). (DOI: 10.1111/j.1466-8238.2011.00739.x)
}

\examples{
require(rangeMapper)
dbcon = rangeMap.start(file = "test.sqlite", overwrite = TRUE, dir = tempdir() )

# Breeding range vector files location
f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")

# Save the global bounding box, 
global.bbox.save(con = dbcon, bbox = f, 
	p4s = CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") ) 

# upload grid size 
gridSize.save(dbcon, gridSize = 200000)  # cell size ~ 200km

#  save canvas 
canvas.save(dbcon) 
summary(canvas.fetch(dbcon) )

# Upload BIO tables
data(wrens)
bio.save(con = dbcon, loc = wrens,  ID = "sci_name")

# Process species ranges
r = readOGR(f, "wrens", verbose = FALSE)

processRanges(spdf = r, con =  dbcon, ID = "sci_name" )


#Using sqlite aggregate functions
rangeMap.save(dbcon, FUN = "median" , biotab = "wrens", 
			biotrait = "body_size", tableName = "body_size")

# Fetch maps

summary(rangeMap.fetch(dbcon) )
 
 
\dontrun{
# import raster maps the current project
require(rangeMapper)
dbcon = rangeMap.start(file = "wrens.sqlite", dir = tempdir(), overwrite = TRUE)
f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
global.bbox.save(con = dbcon, bbox = f, 
p4s = CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
gridSize.save(dbcon)
canvas.save(dbcon)

r = system.file(package = "rangeMapper", "extdata", "etopo1", "etopo1_Americas.tif")
rangeMap.save(dbcon, path = r, tableName = "meanAltitude", FUN = mean, overwrite = TRUE)

m = rangeMap.fetch(dbcon)
plot(m)

} 
 
 
 
# more examples at: http://rangemapper.orn.mpg.de/


}




\author{Mihai Valcu \email{valcu@orn.mpg.de}}
\keyword{spatial}



















