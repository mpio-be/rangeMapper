#' \strong{ra}nge \strong{m}apper \strong{p}ipe line.
#'
#'A quick option to initiate a project by pipelining several functions.
#'
#' @param gridSize grid resolution (in units previously set by \code{global.bbox.save})
#' @param bbox     the spatial domain of the project (see \code{\link{global.bbox.save}} )
#' @inheritParams  rangeMap.start
#' @inheritParams  global.bbox.save
#' @inheritParams  gridSize.save
#' @inheritParams  canvas.save
#' @inheritParams  processRanges
#' @inheritParams  bio.save
#' @inheritParams  rangeMap.save

#' @return an sqlite connection to a rangeMapper project
#' @export
#'
#' @examples
#' breding_ranges = rgdal::readOGR(system.file(package = "rangeMapper",
#'      "extdata", "wrens", "vector_combined"), "wrens", verbose = FALSE)
#' data(wrens)
#' d = subset(wrens, select = c('sci_name', 'body_size', 'body_mass', 'clutch_size') )
#' con = ramp("wrens.sqlite", gridSize = 10, spdf = breding_ranges, biotab = d, ID = "sci_name",
#'             metadata = rangeTraits(), FUN = "median", overwrite = TRUE)
#' m = rangeMap.fetch(con)



ramp <- function(file, dir = tempdir(), gridSize, spdf, bbox = spdf,
                 ID, biotab, metadata, FUN,
                 overwrite = FALSE ) {

        dbcon = rangeMap.start(file = file, dir = dir , overwrite = overwrite)

        global.bbox.save(con = dbcon, bbox = spdf)

        if( missing(gridSize) ) gridSize.save(dbcon) else
            gridSize.save(dbcon, gridSize = gridSize)

        canvas.save(dbcon)

       if( missing(metadata) )  processRanges(con = dbcon, spdf = spdf, ID = ID) else
            processRanges(con = dbcon, spdf = spdf, ID = ID, metadata = metadata)


        bio.save(con = dbcon, loc = biotab,  ID = ID)

        rangeMap.save(dbcon)

        if(!missing(FUN)){
            whichmaps = setdiff(names(biotab), ID)

            n = sapply(whichmaps, function(i)  {
                tabNam = if( inherits(FUN, 'character') ) paste(FUN, i, sep = '_') else i
                rangeMap.save(dbcon, FUN = FUN , biotab = substitute(biotab) %>% deparse ,  biotrait = i, tableName = tabNam)
                }) %>% sum
        }

        new('rangeMap', CON = dbcon) %>% show

        return(dbcon)
    }

