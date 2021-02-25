
#' @importFrom methods as new  validObject
#' @import     graphics
#' @importFrom glue glue 
#' @import     RSQLite
#' @importFrom data.table data.table setorder setDT setnames setcolorder rbindlist timetaken
#' @importFrom future   plan  multisession multicore sequential
#' @importFrom future.apply future_lapply
#' @importFrom progressr handlers progressor with_progress
#' @importFrom sf st_as_sf  st_as_sfc st_as_binary st_crs st_crs<- st_make_grid st_bbox 
#'             st_intersects st_intersection st_drop_geometry st_union st_cast st_agr<-
#'             st_read st_transform
#' @importFrom exactextractr exact_extract
#' @importFrom raster raster 
#' 
#' @importFrom magrittr %>% not is_greater_than
#' @export
magrittr::`%>%`
NULL
#' @export
DBI::dbDisconnect
NULL



utils::globalVariables(c(
    '.',
    '.I',
    '.N',
    'N',
    ':=',
    'con',
    'geometry',
    'bio_id',
    'cell_id', 
    'chunks',
    'i',
    'k',
    'name',
    'pk',
    'row.id',
    'type',
    'chunk',
    'map',
    'rmap_type',
    'sqlite_type', 
    'joinCall',
    'dem'
    ))


.onAttach <- function(libname, pkgname) {
    dcf <- read.dcf(file=system.file("DESCRIPTION", package=pkgname) )
    packageStartupMessage(paste(pkgname, dcf[, "Version"] ))

    }


#'  A Platform for the Study of Macro-Ecology of Life History Traits.
#'
#' @section Package options:
#'
#' rangeMapper global options can be changed with options(name = value):
#'
#' \itemize{
#'   \item `rmap.verbose`: defaults to TRUE. Setting this option to FALSE will shut up all custom messages.
#' }
#' @docType package
#' @keywords internal
#' @name rangeMapper
"_PACKAGE"


.onLoad <- function(libname, pkgname) {

    options(rmap.verbose = TRUE)


    invisible()
    }   



