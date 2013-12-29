
\name{selectShpFiles}
\alias{selectShpFiles}
\alias{rangeFiles}
\alias{rangeFiles-methods}
\alias{rangeFiles,rangeFiles-method}


\title{Select (recursively) shape files}
\description{
Returns the file path to all \sQuote{.shp} polygons in a directory.
}
\usage{
selectShpFiles(dir, \dots)
}

\arguments{
	  \item{dir}{character string specifying the directory containing .shp files.}	
	  \item{\dots}{currently ignored}	
	  
}

\value{
Either a \code{\link{data.frame}} or a character vector is returned.
}	

\note{
The function uses \code{\link[maptools]{getinfo.shape}} to only select polygon files (aka type 5). 
}	



\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\seealso{
	\code{\link{rangeMap.save}}.
	\code{\link[maptools]{getinfo.shape}}
}

\references{
Valcu, M., Dale, J. and Kempenaers, B. (2012)  rangeMapper: A platform for the study of macroecology of life history traits. 21(9). (DOI: 10.1111/j.1466-8238.2011.00739.x)
}
	
	
\examples{
f= system.file(package="rangeMapper", "extdata", "wrens", "vector")
res = selectShpFiles(f, ogr = TRUE, polygons.only = TRUE)
head(res)

}

\keyword{misc}











