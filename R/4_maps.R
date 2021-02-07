
#' Save maps
#'
#' `Maps` are aggregate summaries computed for each canvas cell.  
#' 
#' `rmap_save_map` makes maps based on data within the project or based on external raster objects.
#' Aggregate functions can be: 
#'  * internal `SQL` aggregate functions: `'avg', 'count', 'max', 'min', 'sum', 'stdev', 
#'   'variance', 'mode', 'median', 'lower_quartile', 'upper_quartile', 'group_concat'`.
#'  * `R` functions taking one argument and returning one value. 
#'  * arbitrary statistical models applied on `bio` tables.
#' 
#' 
#' @param con     a rangeMapper connection made with  [rmap_connect()]. 
#' @param fun     the name of the function to save, either an SQLite or an R function.  
#'                see Details. 
#' @param src     the name of the source table previously imported by [rmap_add_bio()]. 
#' @param v       the variable to map or a function taking several variables as arguments.
#'                and returning one or several values.
#' @param subset  the name of a subset table. see \link{rmap_save_subset}.
#' @param dst     the name of the new map table. 
#' @param ...     arguments passed to fun.
#' 
#'
#' @return TRUE when a table or a database view is written to the project file, FALSE otherwise.
#' @export
#' @md
#'
#' @examples
#' require(rangeMapper)
#' require(data.table)
#' con = rmap_connect()
#' data(wrens)
#' rmap_add_ranges(con, x = wrens, ID = 'sci_name')
#' rmap_prepare(con, 'hex', cellsize=500)
#' rmap_save_map(con) # default is a species_richness map.
#' 
#' rmap_add_bio(con, wrens, 'sci_name')
#' rmap_save_map(con, fun='avg', src='wrens',v='body_mass', dst='avg_bodymass')
#' 
#' rmap_save_subset(con,dst ='ss1', species_richness = 'species_richness > 10')
#' rmap_save_map(con,subset = 'ss1', dst ='sr2')
#' rmap_save_map(con, fun='avg', src='wrens',v='body_mass', 
#'  subset='ss1', dst='avg_bodymass_high_SR')
#' 
#' rmap_save_map(con, fun= mean, na.rm = TRUE, src='wrens',
#'  v='body_mass', dst='mean_bodymass')
#' 
#' Median = function(x) median(x,na.rm = TRUE)
#' 
#' rmap_save_map(con, fun = Median, src='wrens',
#'  v='body_mass', dst='median_bodymass')
#' 
#' rmap_save_map(con, fun= mean, na.rm = TRUE, src='wrens',v='body_mass', 
#'  subset='ss1', dst='mean_bodymass_high_SR')
#' 
#' linmod = function(x) {
#'   lm(clutch_size ~ log(female_tarsus), x) %>% 
#'   summary %>% coefficients %>% data.table %>% .[-1] }
#' rmap_save_map(con, fun= linmod, src='wrens', dst='slope_clutch_size')
#' 
#' data(dem)
#' rmap_save_map(con, fun= 'mean', src= dem ,  dst='dem', progress = FALSE)
#' 
#' x = rmap_to_sf(con)
#' 
#' dbDisconnect(con)
#' 
setGeneric("rmap_save_map", function(con, fun, src, v,subset, dst, ...)   standardGeneric("rmap_save_map") )


###### - SQL functions ----------------


#' @rdname rmap_save_map
#' @export
setMethod("rmap_save_map",signature  = c(con='rmapConnection',fun='missing',src='missing',v='missing',subset='missing',dst='missing'),
definition = function(con) {

  if(is_empty(con,'wkt_canvas')) stop('canvas table is empty. Did you run rmap_prepare()?')

  drop_table_or_view('species_richness', con)  

  sql = 'CREATE VIEW IF NOT EXISTS species_richness AS 
          SELECT count(cell_id) species_richness, cell_id from canvas_ranges 
              GROUP BY cell_id'


  dbExecute(con, sql)  

  write_master(con, 'map', 'species_richness', 'COUNT')


   }
  )

#' @rdname rmap_save_map
#' @export
setMethod("rmap_save_map",signature  = c(con='rmapConnection',fun='missing',src='missing',v='missing',subset='character',dst='character'),
definition = function(con, subset, dst) {

  drop_table_or_view(dst, con) 


  sql = glue('CREATE VIEW IF NOT EXISTS {dst} AS 
          SELECT count(cell_id) species_richness_{dst}, cell_id from {subset} 
              GROUP BY cell_id')

  dbExecute(con, sql)  

  write_master(con, 'map', dst, 'COUNT')


   }
  )


#' @rdname rmap_save_map
#' @export
setMethod("rmap_save_map",signature  = c(con='rmapConnection',fun='character',src='character',v='character',subset='missing',dst='character'),
definition = function(con, fun, src, v, dst) {

  sqlite_aggregate_fun = 
      c('avg', 'count', 'max', 'min', 'sum', 
        'stdev', 'variance', 'mode', 'median', 
        'lower_quartile', 'upper_quartile', 
        'group_concat' ) # TODO add derived functions ?

  fun = tolower(fun)    
  if( !fun  %in% sqlite_aggregate_fun ) 
      stop( glue('{ dQuote(fun) } should be one of { paste(sqlite_aggregate_fun,collapse = ",") }') )

  drop_table_or_view(dst, con) 

  sql = 
  glue("CREATE VIEW IF NOT EXISTS {dst} AS
            SELECT {fun}({v}) as {fun}_{v}, cell_id from 
              (SELECT b.{v}, b.bio_id, c.cell_id from {src} b 
                  JOIN canvas_ranges c on b.bio_id = c.bio_id) x
                      GROUP BY cell_id")


  dbExecute(con, sql)  

  write_master(con, 'map', dst, fun)


  }
  )


#' @rdname rmap_save_map
#' @export
setMethod("rmap_save_map",signature  = c(con='rmapConnection',fun='character',src='character',v='character',subset='character',dst='character'),
definition = function(con, fun, src, v,subset, dst) {

  sqlite_aggregate_fun = 
      c('avg', 'count', 'max', 'min', 'sum', 
        'stdev', 'variance', 'mode', 'median', 
        'lower_quartile', 'upper_quartile', 
        'group_concat' ) # todo add derived functions ?

  fun = tolower(fun)    
  if( !fun  %in% sqlite_aggregate_fun ) 
      stop( glue('{ dQuote(fun) } should be one of { paste(sqlite_aggregate_fun,collapse = ",") }') )


  drop_table_or_view(dst, con) 


  sql = 
  glue("CREATE VIEW IF NOT EXISTS {dst} AS
            SELECT {fun}({v}) as {fun}_{v}_{subset}, cell_id from 
              (SELECT b.{v}, b.bio_id, c.cell_id from {src} b 
                  JOIN {subset} c on b.bio_id = c.bio_id) x
                      GROUP BY cell_id")


  dbExecute(con, sql)  

  write_master(con, 'map', dst, 'rangeMapper::rmap_save_map(con, SQL fun, src, v,subset, dst)')


  }
  )

###### - R functions ----------------

.rmap_save <- function(con, fun, src, v, dst, subset, vname, ...) {
  
  drop_table_or_view(dst, con) 

  cr  = dbReadTable(con, subset )  %>% setDT
  bio = dbGetQuery(con, glue('SELECT bio_id, {v} FROM {src}'))  %>% setDT
  cr = merge(cr, bio, by = 'bio_id')

  o = cr[, fun( get(v), ...) , by = cell_id]
  setnames(o, 'V1', vname)


  o1 = dbWriteTable(con,dst, o, row.names = FALSE)


  o2 = write_master(con, 'map', dst, paste(deparse(fun), collapse = '\n') )

  invisible(all(o1, o2))


  }


#' @rdname rmap_save_map
#' @export
setMethod("rmap_save_map",signature  = c(con='rmapConnection',fun='function',src='character',v='character',subset='missing',dst='character'),
definition = function(con, fun, src, v, dst, ...) {

  vn = glue('V1_{v}')
  
  .rmap_save(con = con, fun = fun, ..., src = src , v = v, subset = 'canvas_ranges', dst = dst, vname = vn)

  }
  )


#' @rdname rmap_save_map
#' @export
setMethod("rmap_save_map",signature  = c(con='rmapConnection',fun='function',src='character',v='character',subset='character',dst='character'),
definition = function(con, fun, src, v, subset, dst, ...) {

   vn = glue('V1_{v}')
 
  .rmap_save(con = con, fun = fun, ..., src = src , v = v, subset = subset, dst = dst, vname = vn)



  }
  )


.rmap_save2 <- function(con, fun, src, dst, subset, ...) {
  
  drop_table_or_view(dst, con) 

  cr  = dbReadTable(con, subset )  %>% setDT
  bio = dbGetQuery(con, glue('SELECT *  FROM {src}'))  %>% setDT
  cr = merge(cr, bio, by = 'bio_id')


  o = future_lapply( unique(cr$cell_id), 
      function(i){
        x = cr[cell_id == i]
        E = try(fun(x, ...), silent = TRUE)
        E = data.table(E)
        E[, cell_id := i]
        E

        }, future.seed = TRUE )

  o = rbindlist(o, fill = TRUE) 

  if( nrow( o[ ,.N, cell_id][N>1] ) > 0 ) 
    warning( glue('fun() returned more than one raw. Map {dQuote(dst)} has duplicated cells.')   )


  setnames(o, names(o)  %>%  make_sql_nams)
  

  o1 = dbWriteTable(con,dst, o, row.names = FALSE)


  o2 = write_master(con, 'map', dst, paste(deparse(fun), collapse = '\n')  )

  invisible(all(o1, o2))


  }

#' @rdname rmap_save_map
#' @export
setMethod("rmap_save_map",signature  = c(con='rmapConnection',fun='function',src='character',subset='missing',dst='character'),
definition = function(con, fun, src, v, dst, ...) {

 
  .rmap_save2(con = con, fun = fun, ..., src = src , subset = 'canvas_ranges', dst = dst)

  }
  )


#' @rdname rmap_save_map
#' @export
setMethod("rmap_save_map",signature  = c(con='rmapConnection',fun='function',src='character', subset='character',dst='character'),
definition = function(con, fun, src, v, subset, dst, ...) {

 
  .rmap_save2(con = con, fun = fun, ..., src = src , subset = subset, dst = dst)



  }
  )


#' @rdname rmap_save_map
#' @export
setMethod("rmap_save_map",signature  = c(con='rmapConnection',fun='character',src='Raster',v='missing',subset='missing',dst='character'),
definition = function(con, fun = 'mean', src, dst, ...) {

  drop_table_or_view(dst, con) 

  cnv = rmap_to_sf(con, 'wkt_canvas')

  o1 = exact_extract(src, cnv, fun = fun, force_df = TRUE, ... )  %>% data.table
  o1[, cell_id := cnv$cell_id]

  o1 = dbWriteTable(con,dst, o1, row.names = FALSE)

  o2 = write_master(con, 'map', dst, paste(substitute(dem), class(dem), sep= ': ')  )

  invisible(all(o1, o2))


  }
  )



