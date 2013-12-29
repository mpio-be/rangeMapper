\name{gridSize.save}
\alias{gridSize.save}
\alias{gridSize.fetch}
\alias{gridSizeFetch}
\alias{gridSizeSave}
\alias{gridSizeSave-methods}
\alias{gridSizeSave,gridSize-method}
\alias{gridSizeFetch-methods}
\alias{gridSizeFetch,rangeMap-method}

\title{Save or retrieve the grid size from an \code{rangeMapper} project.}
\description{
Save or retrieve the grid size from the active sqlite database.
}
\usage{
gridSize.save(con, \dots) 
gridSize.fetch(con)
}

\arguments{
	  \item{con}{A connection pointing to a valid \code{rangeMapper} project.}
	  \item{\dots}{ \code{gridSize}: A numeric vector of one unit length. See notes.}
	}

\value{
	\item{\code{gridSize.fetch}}{Returns a numeric vector of one unit length containing the grid size previously saved by \code{gridSize.save}}
}	

\note{
If \code{gridSize} is not given the default grid size is computed based on the bounding box as the 
	range of the smallest axis /100.

}

\seealso{
\code{\link[rangeMapper]{rangeMap.save}}.
\code{\link[rangeMapper]{global.bbox}}
}

\references{
Valcu, M., Dale, J. and Kempenaers, B. (2012)  rangeMapper: A platform for the study of macroecology of life history traits. 21(9). (DOI: 10.1111/j.1466-8238.2011.00739.x)
}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}


\examples{
require(rangeMapper)
wd = tempdir()
dbcon = rangeMap.start(file = "test.sqlite", overwrite = TRUE, dir = wd )
global.bbox.save(con = dbcon)
gridSize.save(dbcon, gridSize = 2)

dbcon = rangeMap.start(file = "test.sqlite", overwrite = TRUE, dir = wd )
global.bbox.save(con = dbcon)
gridSize.save(dbcon)
gridSize.fetch(dbcon) #default grid size value



}

\keyword{spatial}
\keyword{sqlite}









