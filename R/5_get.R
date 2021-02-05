
#' Get `sf` data.frame-s. 
#' 
#' Convert `rangeMapper` to `sf`. 
#' 
#' `rmap_to_sf()` retrieves one of the project's system tables: 
#' `wkt_canvas`, `wkt_ranges` or `bbox` or one or several `map`-s tables. 
#'
#' @param con       a rangeMapper connection made with  [rmap_connect()].
#' @param src       the name of the source table. If missing all `map`s are returned. 
#' @param pattern   a string that identifies several map names. It can be a regular expression.
#' 
#'
#' @return an \link[sf]{sf} data.frame. 
#' @export
#' @md
#' @examples
#' 
#' con = rmap_connect()
#' rmap_add_ranges(con, x = wrens, ID = 'sci_name')
#' rmap_prepare(con, 'hex', cellsize = 500, chunksize = 1)
#' rmap_save_map(con) # default is a species_richness map.
#' rmap_save_subset(con, dst = 'ss1', species_richness = 'species_richness > 5')
#' rmap_save_map(con, subset = 'ss1', dst = 'species_richness_min5')
#' x = rmap_to_sf(con)
#' x = rmap_to_sf(con, 'species_richness_min5')
#' 
#' dbDisconnect(con)

setGeneric("rmap_to_sf", function(con, src,pattern)   standardGeneric("rmap_to_sf") )


.map_as_sf <- function(con, map_names, crs) {

    o = lapply(map_names, function(x) dbReadTable (con, x)  %>% setDT )
    o = Reduce(function(x,y) merge(x = x, y = y, by = "cell_id", all = TRUE), o)

    geom = dbReadTable(con, 'wkt_canvas')  %>% setDT
    o = merge(o, geom, by = 'cell_id')
    o = st_as_sf(o)
    st_crs(o) <- crs

    o 
    }


#' @rdname rmap_to_sf
#' @export
setMethod("rmap_to_sf",signature  = c(con='rmapConnection',src='missing', pattern = 'missing'),
definition = function(con) {
 
    stopifnot( inherits(con, "rmapConnection"))
    crs = dbGetQuery(con, 'select crs from rmap_nfo')$crs

    master = dbGetQuery(con, 'select  name from rmap_master WHERE type = "map"')
    if(nrow(master) == 0) stop('This project does not have any maps.')

    .map_as_sf(con, master$name, crs)    

    })


#' @rdname rmap_to_sf
#' @export
setMethod("rmap_to_sf",signature  = c(con='rmapConnection',src='character', pattern = 'missing'),
definition = function(con, src) {

    stopifnot( inherits(con, "rmapConnection"))
    crs = dbGetQuery(con, 'select crs from rmap_nfo')$crs

    master = dbReadTable(con, 'rmap_master') %>% setDT



    if(length(src)==1 && src == 'wkt_ranges'){
        if(is_empty(con,'wkt_ranges')) stop('Ranges table is empty. Did you run rmap_prepare() ?')

        o = dbReadTable(con, 'wkt_ranges')  %>% st_as_sf
        st_crs(o) <- crs
        } else

    if(length(src)==1 && src =='wkt_canvas'){
        if(is_empty(con,'wkt_canvas')) stop('canvas table is empty. Did you run rmap_prepare()?')

        o = dbGetQuery(con, 
            'SELECT c.geometry, c.cell_id from wkt_canvas c join 
                (SELECT DISTINCT cell_id FROM canvas_ranges) r ON
                    c.cell_id = r.cell_id')  %>% st_as_sf
        st_crs(o) <- crs
        } else

    if(length(src)==1 && src =='bbox') {
        if(is_empty(con,'wkt_ranges')) stop('There is no bounding box because ranges table is empty. Did you run rmap_prepare() ?')

        i = dbGetQuery(con, 'select * from rmap_nfo')
        o = st_bbox(c(xmin = i$xmin, xmax = i$xmax, ymax = i$ymax, ymin = i$ymin), 
                    crs = st_crs(i$crs)) %>% 
             st_as_sfc  %>% 
             st_as_sf 
            } else
    
    if( any(master[name %in% src]$type == 'map')  ) {
        maps = master[name %in% src & type == 'map', name]
        o = .map_as_sf(con, maps,crs)   
        } else

    stop( glue('{src} cannot be converted into a sf object.'))    


    o
    
    })



#' @rdname rmap_to_sf
#' @export
setMethod("rmap_to_sf",signature  = c(con='rmapConnection',src='missing', pattern = 'character'),
definition = function(con, pattern) {
 
    stopifnot( inherits(con, "rmapConnection"))
    crs = dbGetQuery(con, 'select crs from rmap_nfo')$crs

    x = dbGetQuery(con, 'select  name from rmap_master WHERE type = "map"')  %>% setDT
    if(nrow(x) == 0) stop('This project does not have any maps.')

    x = x[ grep(pattern, name) ]    

    if(nrow(x) == 0) stop( glue('The pattern {dQuote(pattern)} does not return any maps.') )

    .map_as_sf(con, x$name, crs)    

    })

