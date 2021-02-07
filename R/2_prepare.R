
#' Define a canvas and process ranges
#' 
#' `rmap_prepare` updates a 'raw' unprepared project to a ready to use project. 
#' `rmap_prepare` creates the project's canvas and assign each range to its corresponding canvas cells by
#' performing a spatial intersection between the ranges and the canvas. The canvas is a regular grid
#' of squares or hexagons. 
#' 
#' Because `rmap_prepare` can be potentially time consuming it can be run in parallel using the 
#' support provided by the `future` package.  
#' `future` allows parallel processing on a variety of systems including high performance computing environments.
#' For details see [future::plan()].
#' Additionally, you can keep track of of the computations using [progressr::handlers()]. 
#' 
#' @param con         a `rangeMapper` connection made with  [rmap_connect()]. 
#' @param grid_type   character "hex" (default) or "square", see [sf::st_make_grid()]. 
#' @param cellsize    target cellsize, see [sf::st_make_grid()].
#' @param chunksize   parallel processing chunk size expressed as proportion from the range size. Default to 1/10. 
#' @param ...         further arguments passed to [sf::st_make_grid()]
#' 
#'
#' @return TRUE when the table is written to the project file, FALSE otherwise. 
#' @export
#' @md 
#' 
#' @references 
#' Birch, C. P., Oom, S. P., & Beecham, J. A. (2007). Rectangular and hexagonal grids used for observation,
#' experiment and simulation in ecology. Ecological modelling, 206(3-4), 347-359.
#' 
#' @seealso  [rmap_add_ranges()]
#' 
#' @examples
#' 
#' # IN-MEMORY PROJECT
#' require(rangeMapper)
#' con = rmap_connect() 
#' rmap_add_ranges(con, wrens, 'sci_name')
#' rmap_prepare(con, 'hex', cellsize=500)
#' dbDisconnect(con)
#' 
#'\dontrun{
#' 
#' # PROJECT PREPARED IN PARALLEL
#' 
#' require(future) 
#' require(progressr)
#' plan(multisession, workers = 2)
#' handlers(global = TRUE)
#' 
#' Path = tempfile() 
#' con = rmap_connect(Path)
#' rmap_add_ranges(con, wrens, 'sci_name')
#' 
#' rmap_prepare(con, 'hex', cellsize=200, chunksize = 0.1)
#' 
#' dbDisconnect(con)
#' plan(sequential)
#' }
#' 

setGeneric("rmap_prepare", function(con, grid_type, cellsize, chunksize, ...)   standardGeneric("rmap_prepare") )


rmap_refresh <- function(con) {
    if( ! is_empty(con, 'wkt_canvas') ) {
        message(glue('    (i) The canvas of project { dQuote(basename(con@dbname)) } is overwritten.'))

        dbExecute(con, 'DELETE FROM wkt_canvas')
        dbExecute(con, 'DELETE FROM canvas_ranges')
        dbExecute(con, 'DELETE FROM rmap_master 
                            WHERE name in ("wkt_canvas", "canvas_ranges", "map", "subset")')
        }


    maps = get_master(con)[rmap_type == 'map']

    if(nrow(maps) > 0) {
        message(glue('    (i) {nrow(maps)} maps are removed.'))
        maps[, i := .I]
        maps[, drop_table_or_view(name, con), by = i]
        }

    dbExecute(con, 'PRAGMA OPTIMIZE')

    }

# make_regular_canvas(con, 'hex', 5000)
make_regular_canvas <- function(con, grid_type, cellsize, ...) {
    if( talk() ) message(glue(" --> Making {grid_type} canvas ..."), appendLF=FALSE)

    bb = rmap_to_sf(con, 'bbox')   

    gt = if(grid_type=='square')  TRUE else if(grid_type=='hex') FALSE else 
         stop("Unknown grid_type!")

    cnv = st_make_grid(bb, cellsize  = cellsize, square = gt, ...)  %>% st_as_sf 
    cnv$cell_id = 1:nrow(cnv)

    if( talk() ) { 
    message("done.")
    message( glue("    (i) The canvas has {nrow(cnv)} cells." ) )
    }

    
    cnv 
    }   

rmap_master_update <- function(con, grid_type, cellsize) {

    if( talk() ) message(" --> Updating rmap_nfo & rmap_master tables ...", appendLF = FALSE)

    sql = glue('UPDATE rmap_nfo SET 
                grid_type = {grid_type %>% shQuote}, 
                cell_size = {cellsize}')
    oi   = dbExecute(con, sql)
    oii  = write_master(con, 'wkt',    'wkt_canvas',    'rangeMapper::rmap_prepare()')
    oiii = write_master(con, 'canvas', 'canvas_ranges', 'rangeMapper::rmap_prepare()')
    
    if( talk() ) message( 'done.')

    all(oi, oii, oiii)


    }

sequential_process_ranges  <- function(con, cnv) {

    r = dbGetQuery(con, 'select bio_id, geometry from wkt_ranges' )

    if( talk() ) message(glue(" --> Processing {nrow(r)} ranges ..."), appendLF=FALSE)

    r = st_as_sf(r)
    st_crs(r) <- st_crs(cnv)

    o = st_intersects(r, cnv)  %>% as.data.frame  %>% setDT

    r = st_drop_geometry(r)  %>% setDT
    r[, k := .I]

    cara = merge(o, r, by.x = 'row.id', by.y = 'k',sort = FALSE)
    cara[, row.id := NULL]
    setnames(cara, 'col.id', 'cell_id')

    return(cara)

    if( talk() ) message('done.')

    }   

parallel_process_ranges <- function(con, chunksize,cnv) {

    # set chunks
    x = dbGetQuery(con, 'select bio_id, pk FROM wkt_ranges') %>% setDT

    csize = ceiling(chunksize*nrow(x))
    n_chuncks  = ceiling(nrow(x) / csize)

    x[ , chunk   := rep(1:n_chuncks, each = csize)[.I] ]

    if( talk() ) message(glue(" --> Processing {nrow(x)} ranges using {n_chuncks} chunks ..."), appendLF=FALSE)

    range_cnv_intersection = function(which_chunk) {

        coni = rmap_connect(con@dbname)

        rpk = x[chunk == which_chunk, range(pk)]

        r = dbGetQuery(coni, 
            glue('select bio_id, geometry from wkt_ranges 
                        WHERE  pk BETWEEN {rpk[1]} AND {rpk[2]}' ) )

        dbDisconnect(coni)

        r = st_as_sf(r)

        st_crs(r) <- st_crs(cnv)

        o = st_intersects(r, cnv)  %>% as.data.frame  %>% setDT

        r = st_drop_geometry(r)  %>% setDT
        r[, k := .I]

        cara = merge(o, r, by.x = 'row.id', by.y = 'k',sort = FALSE)
        cara[, row.id := NULL]
        setnames(cara, 'col.id', 'cell_id')

        cara

        }

    CARA =     
    with_progress({
        p <- progressor(steps = n_chuncks)
        
        future_lapply(1:n_chuncks, function(x) {
            p()
            range_cnv_intersection(x)
            }, future.seed = TRUE) 

        })   
    
    return(rbindlist(CARA))

    if( talk() ) message('done.')                           
    }



#' @rdname rmap_prepare
#' @export
setMethod('rmap_prepare',signature  = c(con='rmapConnection',grid_type='character',cellsize='numeric', chunksize='missing'),

definition = function(con, grid_type = 'hex',cellsize, ...) {

    if( talk() ) tic = proc.time()

    stopifnot( inherits(con, "rmapConnection"))

    rmap_refresh(con)

    # make canvas
        cnv = make_regular_canvas(con, grid_type, cellsize, ...)
        x = data.table(cnv)
        setnames(x, 'x', 'geometry')
        x[, geometry := st_as_binary(geometry)]  

        o1 = dbWriteTable(con, 'wkt_canvas', x, row.names = FALSE, append =  TRUE)

    # process ranges (ranges over canvas)
        CARA =   sequential_process_ranges(con, cnv)  

        o2 = dbWriteTable(con, 'canvas_ranges', CARA, row.names = FALSE, append =  TRUE)
       

    # update rmap_nfo
       o3 = rmap_master_update(con, grid_type, cellsize)
       
       if( talk() ) message ( glue('    (i) Finished in {timetaken(tic)}')  )



    invisible(all(o1, o2, o3))

    } )


#' @rdname rmap_prepare
#' @export
setMethod('rmap_prepare',signature  = c(con='rmapConnection',grid_type='character',cellsize='numeric', chunksize='numeric'),

definition = function(con, grid_type = 'hex',cellsize, chunksize = 1/10, ...) {

    if( talk() ) tic = proc.time()

    stopifnot( inherits(con, "rmapConnection"))

    if(con@dbname==":memory:") {
        stop('In-memory files do not support parallel processing.')
        }

    if(chunksize <= 0 | chunksize >= 1) {
        stop('chunksize should be in the (0,1) interval.')
        }

    rmap_refresh(con)


    # make canvas
        cnv = make_regular_canvas(con, grid_type, cellsize, ...)
        x = data.table(cnv)
        setnames(x, 'x', 'geometry')
        x[, geometry := st_as_binary(geometry)]  
        o1 = dbWriteTable(con, 'wkt_canvas', x, row.names = FALSE, append =  TRUE)


    # process ranges (ranges over canvas)
        CARA = parallel_process_ranges(con, chunksize,cnv)
        o2 = dbWriteTable(con, 'canvas_ranges', CARA, row.names = FALSE, append =  TRUE)


    # update rmap_nfo
       o3 = rmap_master_update(con, grid_type, cellsize)
       
       if( talk() ) message ( glue('    (i) Finished in {timetaken(tic)}')  )



    invisible(all(o1, o2, o3))

    } )













  






