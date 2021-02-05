
#' @importFrom methods as new  validObject
#' @import     graphics
#' @importFrom glue glue 
#' @import     RSQLite
#' @importFrom data.table data.table setDT setnames setcolorder rbindlist timetaken
#' @importFrom future   plan  multisession multicore sequential
#' @importFrom future.apply future_lapply
#' @importFrom progressr handlers progressor with_progress
#' @importFrom sf st_as_sf  st_as_sfc st_as_binary st_crs st_crs<- st_make_grid st_bbox st_intersects st_drop_geometry
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


