
#' Define a canvas and process ranges
#' 
#' `rmap_prepare` updates a 'raw' project containing ranges( [rmap_add_ranges()] ) to a ready to use project. 
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
#'                    When running sequential processing `chunksize` should be set to 1. See details.  
#' @param verbose     default to TRUE. Narrate what is happening. 
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
#' @examples
#' 
#' # SIMPLE IN-MEMORY PROJECT
#' require(rangeMapper)
#' con = rmap_connect() 
#' rmap_add_ranges(con, wrens, 'sci_name')
#' rmap_prepare(con, 'hex', cellsize=500, chunksize = 1)
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
rmap_prepare <- function(con, grid_type = 'hex', cellsize, chunksize = 1/10, verbose = TRUE, ... ) {

    if(verbose) tic = proc.time()

    dbpath = con@dbname    

    # checks
        stopifnot( inherits(con, "rmapConnection"))

        if( ! is_empty(con, 'wkt_canvas') ) {
            message(glue('    (i) The canvas of project { dQuote(dbpath %>% basename) } is overwritten.'))

            dbExecute(con, 'DELETE FROM wkt_canvas')
            dbExecute(con, 'DELETE FROM canvas_ranges')
            dbExecute(con, 'DELETE FROM rmap_master 
                                WHERE name in ("wkt_canvas", "canvas_ranges", "map", "subset")')

            maps = get_master(con)[rmap_type == 'map']
            maps[, i := .I]
            maps[, sql := paste('drop', sqlite_type, name), by = i]
            
            if(nrow(map)>0)
                maps[, dbExecute(con, sql), by = i]

            dbExecute(con, 'PRAGMA OPTIMIZE')
            }

    # make canvas
        if(verbose) message(glue(" --> Making {grid_type} canvas ..."), appendLF=FALSE)
        
        bb = rmap_to_sf(con, 'bbox')   

        gt = if(grid_type=='square')  TRUE else if(grid_type=='hex') FALSE else 
                 stop("Unknown grid_type!")

        cnv = st_make_grid(bb, cellsize  = cellsize, square = gt, ...)  %>% st_as_sf 
        cnv$cell_id = 1:nrow(cnv)
        
        x = data.table(cnv)
        setnames(x, 'x', 'geometry')
        x[, geometry := st_as_binary(geometry)]
        o1 = dbWriteTable(con, 'wkt_canvas', x, row.names = FALSE, append =  TRUE)
        
        if(verbose) { 
            message("done.")
            message( glue("    (i) The canvas has {nrow(x)} cells." ) )
            }



    # process ranges (ranges over canvas)
        # set chunks
        x = dbGetQuery(con, 'select bio_id, pk FROM wkt_ranges') %>% setDT

        csize = ceiling(chunksize*nrow(x))
        n_chuncks    = ceiling(nrow(x) / csize)

        if(chunksize <= 0) n_chuncks = nrow(x)
        if(chunksize > 1 ) n_chuncks = 1


        x[ , chunk   := rep(1:n_chuncks, each = csize)[.I] ]

        if(dbpath==":memory:" & n_chuncks > 1) {
            n_chuncks  = 1
            x[, chunk := 1]
            warning('In-memory files do not support parallel processing, chunksize is set to 1')
            }

        if(verbose) message(glue(" --> Processing {nrow(x)} ranges using {n_chuncks} chunks ..."), appendLF=FALSE)

        run_range_cnv_intersection = function(ck) {
            
            if(n_chuncks >  1) coni = rmap_connect(dbpath)
            if(n_chuncks == 1) coni = con

             rpk = x[chunk==ck, range(pk)]

            r = dbGetQuery(coni, glue('select bio_id, geometry from wkt_ranges WHERE  pk BETWEEN {rpk[1]} AND {rpk[2]}' ) )
            
            if(n_chuncks >  1) dbDisconnect(coni)

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

        if(n_chuncks > 1) {
            CARA =     
            with_progress({
                p <- progressor(steps = n_chuncks)
                
                future_lapply(1:n_chuncks, function(x) {
                    p()
                    run_range_cnv_intersection(x)
                    }, future.seed = TRUE) 

                })   
                                    
            CARA = rbindlist(CARA)
            }    

        if(n_chuncks == 1) 
            CARA = run_range_cnv_intersection(1)

        o2 = dbWriteTable(con, 'canvas_ranges', CARA, row.names = FALSE, append =  TRUE)

        if(verbose) message('done.')

    # update rmap_nfo
       if(verbose) message(" --> Updating rmap_nfo & rmap_master tables ...", appendLF=FALSE)

        sql = glue('UPDATE rmap_nfo SET 
                    grid_type = {grid_type %>% shQuote}, 
                    cell_size = {cellsize}
                    ')
        o3 = dbExecute(con, sql)

        # rmap_master
        o4=write_master(con, 'wkt',    'wkt_canvas',    'rangeMapper::rmap_prepare()')
        o5=write_master(con, 'canvas', 'canvas_ranges', 'rangeMapper::rmap_prepare()')
                    
       
       if(verbose) {
        message( 'done.')

        message ( glue('    (i) Finished in {timetaken(tic)}')  )
        
        }


    invisible(all(o1, o2, o3,o4,o4,o5))

    }













  






