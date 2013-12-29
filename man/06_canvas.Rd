\name{canvas.save}
\alias{canvas.save}
\alias{canvas.fetch}
\alias{canvasFetch}
\alias{canvasSave}
\alias{canvasFetch-methods}
\alias{canvasFetch,rangeMap-method}
\alias{canvasSave-methods}
\alias{canvasSave,rangeMap-method}

\title{Project's canvas}

\description{
	The canvas is a  regular grid of a given resolution. Each range map is overlayed onto the canvas and the results saved to project.}

\usage{
canvas.save(con)
canvas.fetch(con)
}

\arguments{
	  \item{con}{An sqlite connection pointing to a valid \code{rangeMapper} project. }

}  

\value{
	\code{canvas.fetch} Returns a \code{\link[sp]{SpatialPixelsDataFrame}} object.
}

\note{
The method canvasSave() fails if \code{grid.size} was not set and if the canvas was already constructed for the given project.
}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\seealso{
	\code{\link{rangeMap.save}}.\cr
	\code{\link{gridSize.save}} 
	}
	
\references{
Valcu, M., Dale, J. and Kempenaers, B. (2012)  rangeMapper: A platform for the study of macroecology of life history traits. 21(9). (DOI: 10.1111/j.1466-8238.2011.00739.x)
}
	
\examples{
require(rangeMapper)
wd = tempdir()
dbcon = rangeMap.start(file = "test.sqlite", overwrite = TRUE, dir = wd)
global.bbox.save(con = dbcon)
gridSize.save(dbcon, gridSize = 2)
canvas.save(dbcon)
cnv = canvas.fetch(dbcon)
summary(cnv)
plot(cnv, col = 'grey', axes = TRUE)
}

\keyword{spatial}
\keyword{sqlite}



















