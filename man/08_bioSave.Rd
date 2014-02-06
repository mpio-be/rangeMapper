
\name{bio.save}
\alias{bioSave}
\alias{bio.save}
\alias{metadata2bio}
\alias{bio.merge}
\alias{bioSave-methods}
\alias{bioSave,bioSaveDataFrame-method}
\alias{bioSave,bioSaveFile-method}

\title{Import \sQuote{BIO} tables to  a \code{rangeMapper} project.}
\description{
Import tables (e.g. life history data) to an active \code{rangeMapper} project.
}
	
\usage{
	bio.save(con, loc, tableName, \dots)
	metadata2bio(con, \dots)
	bio.merge(con, tableName, \dots)
}



\arguments{
	\item{con}{An sqlite connection pointing to a valid \code{rangeMapper} project. }
	\item{loc}{file location or \code{data.frame} name}
	\item{tableName}{if missing, the name of the file or data.frame is used}
	\item{\dots}{Arguments to pass to the corresponding methods: 
				  e.g. the ID, the column corresponding to the names of the range files}
}

\value{
A \sQuote{BIO} table is created in the corresponding \code{rangeMapper} project. 
}



\seealso{
\code{\link{rangeMap.save}}.
\code{\link{wrens}}
}

\references{
Valcu, M., Dale, J. and Kempenaers, B. (2012)  
rangeMapper: A platform for the study of macroecology of life history traits. 21(9). 
(DOI: 10.1111/j.1466-8238.2011.00739.x)
}


\examples{
require(rangeMapper)
wd = setwd(tempdir())
r = readOGR(system.file(package = "rangeMapper", 
	"extdata", "wrens", "vector_combined"), "wrens", verbose = FALSE)
dbcon = rangeMap.start(file = "wrens.sqlite", overwrite = TRUE, 
	dir = tempdir() )
global.bbox.save(con = dbcon, bbox = r)
gridSize.save(dbcon, gridSize = 2)  
canvas.save(dbcon) 
processRanges(spdf = r, con =  dbcon, ID = "sci_name" )

# Upload BIO tables
data(wrens)
Troglodytes  = wrens[grep("Troglodytes", wrens$sci_name), c(2, 5)]
bio.save(con = dbcon, loc = Troglodytes,  ID = "sci_name")

summary(rangeMap("wrens.sqlite"))$BIO_tables
setwd(wd)


\dontrun{
require(rangeMapper)
wd = setwd(tempdir())
r = readOGR(system.file(package = "rangeMapper", 
  "extdata", "wrens", "vector_combined"), "wrens", verbose = FALSE)
dbcon = rangeMap.start(file = "wrens.sqlite", overwrite = TRUE, 
	dir = tempdir() )
global.bbox.save(con = dbcon, bbox = r)
gridSize.save(dbcon, gridSize = 2)  
canvas.save(dbcon) 
processRanges(spdf = r, con =  dbcon, ID = "sci_name", metadata = rangeTraits() )

wrensPath = system.file(package = "rangeMapper", "data", "wrens.csv")
bio.save(con = dbcon, loc = wrensPath,  ID = "sci_name")
bio.merge(dbcon, "wrensNew")
metadata2bio(dbcon)

summary(rangeMap("wrens.sqlite"))
setwd(wd)

}



}



\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\keyword{import}














