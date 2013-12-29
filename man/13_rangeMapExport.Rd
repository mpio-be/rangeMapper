\name{rangeMap.export}
\alias{rangeMap.export}
\alias{rangeMapExport}
\alias{rangeMapExport-methods}
\alias{rangeMapExport,rangeMapExport-methods}
\alias{rangeMapExport,rangeMap,character-method}


\title{
	Export \sQuote{MAP} tables}

\description{
	Export \sQuote{MAP} tables as single-band geotiff files }

	
\usage{
	rangeMap.export(con, dirName, \dots) }

\arguments{
	\item{con}{An sqlite connection pointing to a valid \code{rangeMapper} project. }
	\item{dirName}{The directory name where the \sQuote{MAP}s will be exported. If missing, the \sQuote{MAP}s will be exported in project's directory}
	\item{\dots}{Further arguments to pass to \code{\link{writeGDAL}} }
}	

\references{
Valcu, M., Dale, J. and Kempenaers, B. (2012)  rangeMapper: A platform for the study of macroecology of life history traits. 21(9). (DOI: 10.1111/j.1466-8238.2011.00739.x)
}


\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\keyword{export}




	