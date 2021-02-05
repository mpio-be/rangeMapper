

rmap_skeleton <- function() {

    rmap_nfo = "CREATE TABLE rmap_nfo(
                version         TEXT,   -- package version
                crs             TEXT,   -- crs 
                grid_type       TEXT,   -- hex or square
                cell_size       TEXT,   -- target cell size
                xmin            REAL,   -- bounding 
                ymin            REAL,   -- bounding 
                xmax            REAL,   -- bounding 
                ymax            REAL    -- bounding 
                )"

    rmap_master = "CREATE TABLE rmap_master(
                type            TEXT,   -- wkt, canvas, bio, map, raster, ..
                name            TEXT,   -- table name
                source          TEXT,   -- external file name and/or procedure or R function
                pk              INTEGER PRIMARY KEY AUTOINCREMENT
                )"


    wkt_canvas = "CREATE TABLE wkt_canvas(
                geometry        TEXT,      --sfc geometry
                cell_id         INTEGER PRIMARY KEY -- cell ID
                )"
    wkt_ranges = "CREATE TABLE wkt_ranges(
                geometry        BLOB,      --sfc geometry
                bio_id          TEXT,    -- range ID, same as a BIO_ table ID
                pk              INTEGER PRIMARY KEY -- range ID, same as a BIO_ table ID
                )"
    canvas_ranges = "CREATE TABLE canvas_ranges(
                cell_id         INTEGER,   -- canvas cell ID 
                bio_id          TEXT       -- range ID, same as a BIO_ table ID
               )"




    c(rmap_nfo      = rmap_nfo,  rmap_master = rmap_master, 
      wkt_canvas    = wkt_canvas, wkt_ranges = wkt_ranges,
      canvas_ranges = canvas_ranges)
    }

setClass("rmapConnection",contains = "SQLiteConnection")

setValidity("rmapConnection", 
    function(object) {

        tnams = rmap_skeleton()  %>% names
        ok = tnams %in% dbListTables(object)  %>% all
        if(!ok) {"Invalid rangeMapper project!"} else {

            TRUE
        }
        })

#' rangeMapper connect
#' 
#' Connect to a new or an existing rangeMapper project. 
#' 
#' An empty `rangeMapper` file is an sqlite database with five system tables: 
#'  * __rmap_nfo__        containing the package version, the crs string, the canvas type and the bounding box. 
#'  * __rmap_master__    a table similar with the in-build `sqlite_master` table holding information about the 
#'                         tables created or importing while working on the project.
#'  * __canvas_ranges__  a table that makes the link between the `canvas` and any entities usually species 
#'                        mapped on the canvas. 
#'  * __wkt_canvas__    a table containing the canvas polygons as wkt binary.   
#'  * __wkt_ranges__    a table containing the range  polygons (usually species distribution ranges) as wkt binary.  
#' 
#' If any of system tables is changed or missing then the file is considered corrupted and cannot be open with `rmap_connect()`.
#' 
#' @param path      filepath . When not specified, an `in-memory` file is created.
#' @param overwrite when TRUE, the file is removed and the project re-initiated. 
#'
#' @return an object of class rmapConnection
#' @export
#'
#' @examples
#' con = rmap_connect()
#' class(con)
#' dbDisconnect(con)
#' @md

rmap_connect <- function(path = ":memory:" , overwrite = FALSE) {


    if(overwrite & file.exists(path) ) { 
        file.remove(path)
        }


    # existing connection
     if(file.exists(path) ) {
        con = dbConnect(RSQLite::SQLite(), path)
        }

    # new connection
    if(path == ":memory:" || !file.exists(path) ) {
        con = dbConnect(RSQLite::SQLite(), path)
        sapply(rmap_skeleton(), FUN = dbExecute, conn = con)
        }

    # rmapConnection
    rcon <- as(con,"rmapConnection")
    validObject(rcon)

    rcon

    }


#' Add polygons to a rangeMapper project 
#' 
#' Add polygon ranges (usually species or populations distribution ranges) to a rangeMapper project 
#' 
#' Polygons are saved as WKB (see [sf::st_as_binary()]).
#'
#' @param con  a rangeMapper connection made with   [rmap_connect()].
#' @param x    a spatial polygon object of class `sf` (see [sf::st_as_sf()] ) . 
#' @param ID   character string. name of the ID column, usually species name. 
#'
#' @return TRUE when the table is written to the project file, FALSE otherwise. 
#' @export
#' @md
#'
#' @examples
#' con = rmap_connect()
#' rmap_add_ranges(con, x = wrens, ID = 'sci_name')
#' dbDisconnect(con)
#' 
setGeneric("rmap_add_ranges", function(con,x, ID)   standardGeneric("rmap_add_ranges") )

#' @rdname rmap_add_ranges
#' @export
setMethod("rmap_add_ranges",signature  = c(con = "rmapConnection", x= "sf", ID = "character"),
definition = function(con, x, ID) {

    if( ! is_empty(con, 'wkt_ranges') ) {
        stop('Ranges are already imported here. re-connect with rmap_connect(overwrite = TRUE) to start from scratch.')
         dbDisconnect(con)
        }

    # ranges    
    d = data.table(x)
    setnames(d, ID, 'bio_id')

    wkt_ranges = d[, .(geometry, bio_id)]
    wkt_ranges[, geometry := st_as_binary(geometry)]

    o1 = dbWriteTable(con, 'wkt_ranges', wkt_ranges, row.names = FALSE, append = TRUE)

    # rmap_nfo: crs and box
    # bounding box
    bb = st_bbox(x)
    sql = glue('INSERT INTO rmap_nfo (
                    version,
                    crs,
                    xmin,
                    ymin,
                    xmax,
                    ymax)
                VALUES( 
                    {packageVersion("rangeMapper")  %>% shQuote}, 
                    {st_crs(x)$input  %>% shQuote },
                    {bb$xmin},
                    {bb$ymin},
                    {bb$xmax},
                    {bb$ymax} 
                    )'
                )

    o2 = dbExecute(con, sql)

    # rmap_master
    o3 = write_master(con, 'wkt', 'wkt_ranges', 'rangeMapper::rmap_add_ranges()')

    invisible(all(o1, o2,o3))

     }
    )


#' Add non-spatial tables to a rangeMapper project 
#' 
#' Add any dataset to the project. The dataset is saved in a separate table inside the project and 
#' labelled as a `bio` table. 
#' 
#' The `bio` tables contain the data which is then mapped with [rmap_save_map()] 
#' at each canvas cell and/or data used to create `subsets` with [rmap_save_subset()]. 
#' If the `bio` table inherits from sf then the geometry is silently dropped and only the non-spatial 
#' data are imported.
#'
#' @param con    a rangeMapper connection made with  [rmap_connect()] . 
#' @param x      an object inheriting from [base::data.frame()].
#' @param ID     character string. name of the ID column, usually species name. 
#' @param name   output table name. If name is missing then name is the same as x. 
#'
#' @return TRUE when the table is written to the project file, FALSE otherwise. 
#' @export
#' @md
#' @examples
#' 
#' con = rmap_connect()
#' rmap_add_ranges(con, wrens, 'sci_name')
#' rmap_add_bio(con, wrens, 'sci_name')
#' dbDisconnect(con)
#' 
setGeneric("rmap_add_bio", function(con,x, ID,name)   standardGeneric("rmap_add_bio") )


#' @rdname rmap_add_bio
#' @export
setMethod("rmap_add_bio",signature  = c(con = "rmapConnection", x= "data.table", ID = "character", name = "character"),
    definition = function(con, x, ID, name) {
   
    if( !exists_in_master(con,'wkt_ranges') ) { 
        stop ('This project does not have any ranges. Did you run rmap_add_ranges()?')
        }
    
    if(exists_in_master(con,name) ) { 
        dbExecute(con, glue("drop table {name}"))
        warning ( glue("table {dQuote(name)} exists and it will be overwritten.") )
        }

    setnames(x, ID, 'bio_id')
    setcolorder(x, 'bio_id')

    #cross-check ID: x vs. wkt_ranges
        wr = dbGetQuery(con, "select bio_id from wkt_ranges")
        z = merge(x, wr, by = 'bio_id')

    if(nrow(z) == 0) 
        stop( glue("The ID-s in {dQuote(name)} do not match the ID-s in {dQuote('wkt_ranges')}.") )
    
    if(nrow(z) < nrow(wr)) 
        warning( glue("Partial ID match: There are {nrow(wr)-nrow(z)} more ID-s in {dQuote('wkt_ranges')} than in  {dQuote(name)}.") )


    o1 = dbWriteTable(con, name, x, row.names = FALSE, append = TRUE)    
    o2 = write_master(con, 'bio', name, 'rangeMapper::rmap_add_bio()')

    invisible(all(o1, o2) )

     }
    )

#' @rdname rmap_add_bio
#' @export
setMethod("rmap_add_bio",signature  = c(con = "rmapConnection", x= "ANY", ID = "character", name = "missing"),
    definition = function(con, x, ID, name) {
    
    name = deparse(substitute(x))
    rmap_add_bio(con, x, ID, name)

     }
    )

#' @rdname rmap_add_bio
#' @export
setMethod("rmap_add_bio",signature  = c(con = "rmapConnection", x= "data.frame", ID = "character", name = "character"),
    definition = function(con, x, ID, name) {
    
    setDT(x)
    rmap_add_bio(con, x, ID, name)

     }
    )

#' @rdname rmap_add_bio
#' @export
setMethod("rmap_add_bio",signature  = c(con = "rmapConnection", x= "sf", ID = "character", name = "character"),
    definition = function(con, x, ID, name) {
    
    x = st_drop_geometry(x)  %>% setDT
    rmap_add_bio(con, x, ID, name)

     }
    )














