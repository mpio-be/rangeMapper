\name{rangeMap-class}
\Rdversion{1.1}
\docType{class}
\alias{rangeMap-class}
\alias{summary,rangeMap-method}


\title{Class "rangeMap" to formally describe a rangeMapper sqlite file.}
\description{
This is the basic class of the package.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("rangeMap", \dots)}.
}

\section{Slots}{
  \describe{
	\item{\code{CON}:}{Object of class \code{"SQLiteConnection"} pointing to a \code{rangeMapper} project}
	\item{\code{VERSION}:}{Object of class \code{"character"} corresponding to the \code{rangeMapper} version}
	\item{\code{ID}:}{Object of class \code{"character"}; the ID column corresponding to the names of the imported range files. }
    \item{\code{BIOID}:}{Object of class \code{"character"} the ID column corresponding to the names of imported 	\sQuote{BIO} tables. }
	\item{\code{PROJ4STRING}:}{Object of class \code{"character"} the proj4 string }
	\item{\code{GRIDSIZE}:}{Object of class \code{"character"} grid size (in map units)}
	\item{\code{BBOX}:}{Object of class \code{"character"} bounding box}
    \item{\code{METADATA_RANGES}:}{Object of class \code{"character"} The name of a predefined table optionally constructed at the range import stage}
    \item{\code{CANVAS}:}{Object of class \code{"character"} The name of the table hosting the project's regular grid (the canvas) }
    \item{\code{RANGES}:}{Object of class \code{"character"} The name of the table hosting the ranges}
    \item{\code{BIO}:}{Object of class \code{"character"} The prefix for \sQuote{BIO} tables }
    \item{\code{MAP}:}{Object of class \code{"character"} The prefix for \sQuote{MAP} tables  }
  }
}

\section{Methods}{
  \describe{
    \item{canvasFetch}{\code{signature(object = "rangeMap")}: \dots }
    \item{canvasSave}{\code{signature(object = "rangeMap")}: \dots }
    \item{gridSizeFetch}{\code{signature(object = "rangeMap")}: \dots }
    \item{rangeMapBboxFetch}{\code{signature(object = "rangeMap")}: \dots }
    \item{rangeMapBboxSave}{\code{signature(object = "rangeMap", bbox = "character", p4s = "CRS")}: \dots }
    \item{rangeMapBboxSave}{\code{signature(object = "rangeMap", bbox = "character", p4s = "missing")}: \dots }
    \item{rangeMapBboxSave}{\code{signature(object = "rangeMap", bbox = "missing", p4s = "CRS")}: \dots }
    \item{rangeMapBboxSave}{\code{signature(object = "rangeMap", bbox = "missing", p4s = "missing")}: \dots }
    \item{rangeMapBboxSave}{\code{signature(object = "rangeMap", bbox = "Spatial", p4s = "missing")}: \dots }
    \item{rangeMapExport}{\code{signature(object = "rangeMap", dirName = "character")}: \dots }
    \item{rangeMapProcess}{\code{signature(object = "rangeMap", spdf = "missing", dir = "character", ID = "missing", metadata = "list", parallel = "missing")}: \dots }
    \item{rangeMapProcess}{\code{signature(object = "rangeMap", spdf = "missing", dir = "character", ID = "missing", metadata = "missing", parallel = "missing")}: \dots }
    \item{rangeMapProcess}{\code{signature(object = "rangeMap", spdf = "SpatialPolygonsDataFrame", dir = "missing", ID = "character", metadata = "list", parallel = "missing")}: \dots }
    \item{rangeMapProcess}{\code{signature(object = "rangeMap", spdf = "SpatialPolygonsDataFrame", dir = "missing", ID = "character", metadata = "missing", parallel = "missing")}: \dots }
    \item{summary}{\code{signature(object = "rangeMap")}: \dots }
	 }
}


\author{ Mihai Valcu, \email{valcu@orn.mpg.de}
}


\seealso{
  \code{\link{rangeMapper}},
  \code{\link{processRanges}}

}


\keyword{classes}




