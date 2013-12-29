\name{SpatialPixelsRangeMap}
\alias{SpatialPixelsRangeMap-class}

\Rdversion{1.1}
\docType{class}

\title{Class "SpatialPixelsRangeMap"}
\description{
A class extending \code{"\linkS4class{SpatialPixelsDataFrame}"} used for \sQuote{MAP}s retrieval from a \code{rangeMapper} project
}



\section{Slots}{
  \describe{
	\item{\code{mapvar}:}{Object of class \code{"character"}  }
  }
}
\section{Extends}{
Class \code{"\linkS4class{SpatialPixelsDataFrame}"}, directly.

}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "SpatialPixelsRangeMap", y = "missing")}: ... }
	 }
}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\note{
For the moment \code{mapvar} is the only slot which makes this class distinct from the \code{"\linkS4class{SpatialPixelsDataFrame}"} class.
}


\seealso{
\code{\link{rangeMap.fetch}}
}


\keyword{classes}