\name{rangeMap.start}
\alias{rangeMap.start}
\alias{rangeMapStart}
\alias{rangeMap.open}
\alias{rangeMap}

\title{Initiate/open a new rangeMapper project}

\description{
Initiate/open a new rangeMapper project using a \code{\link{rangeMapStart-class} object} 
}

\usage{
rangeMap.start(\dots)
rangeMap.open(path, verbose)
rangeMap(path)
}

\arguments{
	  \item{path}{Character vector; a path to a valid rangeMapper project}
	  \item{verbose}{Character vector; if \code{TRUE} the project's summary is printed }
	  \item{\dots}{Arguments to be passed to \code{\link{rangeMapStart-class}} }
}

\value{
rangeMap.start() and rangeMap.open() returns an sqlite connection.
rangeMap() returns a \code{\link{rangeMap-class}} object.
}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\seealso{
	\code{\link{rangeMap.save}}.\cr
	\code{\link{rangeMapStart-class}} 
}


\examples{
td = setwd(tempdir())

dbcon = rangeMap.start(file = "test.sqlite", overwrite = TRUE, dir = tempdir() )
summary(dbcon)

summary(rangeMap("test.sqlite"))

dbcon = rangeMap.open(path = "test.sqlite")
summary(dbcon)
setwd(td)

}

\keyword{spatial}

















