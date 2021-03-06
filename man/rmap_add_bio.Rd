% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1_set.R
\name{rmap_add_bio}
\alias{rmap_add_bio}
\alias{rmap_add_bio,rmapConnection,data.table,character,character-method}
\alias{rmap_add_bio,rmapConnection,ANY,character,missing-method}
\alias{rmap_add_bio,rmapConnection,data.frame,character,character-method}
\alias{rmap_add_bio,rmapConnection,sf,character,character-method}
\title{Add non-spatial tables to a rangeMapper project}
\usage{
rmap_add_bio(con, x, ID, name)

\S4method{rmap_add_bio}{rmapConnection,data.table,character,character}(con, x, ID, name)

\S4method{rmap_add_bio}{rmapConnection,ANY,character,missing}(con, x, ID, name)

\S4method{rmap_add_bio}{rmapConnection,data.frame,character,character}(con, x, ID, name)

\S4method{rmap_add_bio}{rmapConnection,sf,character,character}(con, x, ID, name)
}
\arguments{
\item{con}{a rangeMapper connection made with  \code{\link[=rmap_connect]{rmap_connect()}} .}

\item{x}{an object inheriting from \code{\link[base:data.frame]{base::data.frame()}}.}

\item{ID}{character string. name of the ID column, usually species name.}

\item{name}{output table name. If name is missing then name is the same as x.}
}
\value{
TRUE when the table is written to the project file, FALSE otherwise.
}
\description{
Add any dataset to the project. The dataset is saved in a separate table inside the project and
labelled as a \code{bio} table.
}
\details{
The \code{bio} tables contain the data which is then mapped with \code{\link[=rmap_save_map]{rmap_save_map()}}
at each canvas cell and/or data used to create \code{subsets} with \code{\link[=rmap_save_subset]{rmap_save_subset()}}.
If the \code{bio} table inherits from sf then the geometry is silently dropped and only the non-spatial
data are imported.
}
\examples{

con = rmap_connect()
wrens = read_wrens()
rmap_add_ranges(con, wrens, 'sci_name')
rmap_add_bio(con, wrens, 'sci_name')
dbDisconnect(con)

}
