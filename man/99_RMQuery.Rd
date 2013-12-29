
\name{RMQuery}
\alias{RMQuery}


\title{Query a \code{rangeMapper} project}
\description{
For the moment a simple wrapper to sqliteQuickSQL.
}
	
\usage{
	RMQuery(con, statement)
}



\arguments{
	\item{con}{An sqlite connection}
	\item{statement}{An SQL string}
}



\value{
Returns either a data.frame if the statement is a select-like or NULL otherwise.
}



\seealso{
\code{\link{sqliteQuickSQL}}.
}


\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\keyword{SQL}














