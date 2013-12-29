\name{rangeMapStart-class}
\Rdversion{1.1}
\docType{class}
\alias{rangeMapStart-class}
\alias{rangeMapStart,rangeMapStart-method}

\title{Class \code{"rangeMapStart"}}
\description{
Methods of this class allows to initiate a new rangeMapper project.
}


\section{Slots}{
  \describe{
	\item{\code{dir}:}{Object of class \code{"character"}; the directory of the sqlite database.}
	\item{\code{file}:}{Object of class \code{"character"}; sqlite database file-name, If missing a default name is assigned.}
	\item{\code{skeleton}:}{Object of class \code{"list"} containing the sql code to initiate the sqlite database. }
	\item{\code{overwrite}:}{Object of class \code{"logical"} when \code{TRUE} all the database tables are dropped.}
  }
}
\section{Methods}{
  \describe{
    \item{rangeMapStart}{\code{signature(object = "rangeMapStart")}: \dots }
	 }
}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}


\seealso{
	\code{\link{rangeMapper}}\cr
	\code{\link{rangeMap.start}}
	}

\examples{
showClass("rangeMapStart")
str(new("rangeMapStart"))
}


\keyword{classes}









