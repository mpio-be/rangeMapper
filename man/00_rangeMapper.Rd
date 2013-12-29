
\name{rangeMapper}
\alias{rangeMapper}
\docType{package}

\title{rangeMapper: A platform for the study of macroecology of life history traits.}
\description{ 
	\pkg{rangeMapper} is a front end platform for the study of macroecology of life history traits at both inter-specific and assemblage levels. 
	}
\details{
	The package uses species range (extent-of-occurrence) vector polygons and life history traits data-sets to build up maps (e.g. species richness, mean body mass, \dots). \cr
	\pkg{rangeMapper} comes with an user-friendly platform-independent tcltk graphical user interface. 
}
\keyword{package}
\keyword{spatial}

\author{
	Mihai Valcu \email{valcu@orn.mpg.de}, \url{http://orn.mpg.de/mitarbeiter/valcu.html} \cr
	James Dale \email{J.Dale@massey.ac.nz}, \url{http://quelea.net}}


\section{Getting Started}{
	For a quick start open the graphical user interface (\bold{gui}) by typing \code{rangeMapper()}. Mouse over the buttons to see further notes (tool-tips) regarding each step. \cr
	A tutorial can be accessed from Help/\sQuote{Get started} under the \bold{gui} or by browsing the \sQuote{doc} package directory.\cr
	See also the example below in the \emph{examples} section on how to use \pkg{rangeMapper} from the command line.
	}
 
 
\section{The rangeMapper pipeline}{
	 \itemize{
		\item Initiate a new project (an empty sqlite database) using a pre-defined template. 
		\item Extract/define the global bounding box of all selected range maps.
		\item Generate a regular grid (the canvas) using the previously constructed global bounding box.  
		\item Perform polygon-grid overlay of all selected range maps.
		\item Optionally compute pre-defined or user-defined range structure indexes.
		\item Optionally import non-spatial data to be mapped at each grid cell (data are saved as a 'BIO' table to database).		
		\item Optionally import georeferenced raster files.
		\item Compute a chosen statistical model at each grid cell optionally using complex subsets.
		\item  Save, plot, export the MAP-s.
  }
}  

 

\references{
Valcu, M., Dale, J. and Kempenaers, B. (2012)  rangeMapper: A platform for the study of macroecology of life history traits. 21(9). (DOI: 10.1111/j.1466-8238.2011.00739.x)
}

\seealso{
\code{\link[rangeMapper]{rangeMap.save}}.
}


\examples{
\dontrun{ 
rangeMapper()
}
}
 
 

\keyword{macroecology}
\keyword{spatial}
\keyword{gui}
\keyword{tcltk}






