
#' Save subsets
#'
#' `rmap_save_subset` creates subsets based on the canvas properties and/or the properties of one or 
#' several `bio` tables. 
#' 
#' Subsets are defined using `table_name = "CONDITION"` where `CONDITION` can be any `SQL` `WHERE` call
#' defined for the given table.
#' Here is a summary of the [SQL operators](https://www.sqlite.org/lang_expr.html) relevant in this context:
#' 
#' |Operator |Description    | 
#' |---      |---  |
#' | `=` or `==` or `IS`  or `! =` or `<>` or `IS NOT` |  Equals or Non-equals.  |
#' | `>` or `<`  or `>=` or `<=`     |  Greater (Less) than (or equal).  |
#' | `IN` or `NOT IN`  |  multiple given values e.g. `a IN (a,b,c,x,y)`. |
#' | `BETWEEN` |  Between a given range (given values included) e.g. `BETWEEN 1 and 10`. |
#' | `LIKE` |  Pattern search e.g. `LIKE "%keyword%"`. `LIKE` is **case insensitive**. |
#' | `GLOB` |  Similar to `LIKE` but uses the Unix wildcards (`*`,`?`,`[]`). e.g. `[a-zA-Z0-9] matches any single alphanumeric. `GLOB` is **case sensitive**.  |
#' 
#' 
#' @param con     a `rangeMapper` connection made with  [rmap_connect()].
#' @param dst     the name of the new subset table. 
#' @param ...     SQL `WHERE` calls, see Details.
#' 
#'
#' @return TRUE when the database view is written to the project file, FALSE otherwise.
#' @export
#' @md
#' @examples 
#' require(rangeMapper)
#' con = rmap_connect()
#' data(wrens)
#' rmap_add_ranges(con, x = wrens, ID = 'sci_name')
#' rmap_prepare(con, 'hex', cellsize = 500, chunksize = 1)
#' rmap_add_bio(con, wrens, 'sci_name')
#' rmap_save_map(con) 
#' rmap_save_subset(con,'s1', 
#'    species_richness = 'species_richness > 10', 
#'    wrens = 'body_mass > 19 AND clutch_size > 3')
#' 

#' DBI::dbDisconnect(con)
#' 
setGeneric("rmap_save_subset", function(con, dst, ...)   standardGeneric("rmap_save_subset") )


.get_ellipsis <- function( ... ) {
  W = list(...)
  W = lapply(W, as.character)
  data.table(name = names(W), whereCall = W)
  }


#' @rdname rmap_save_subset
#' @export
setMethod("rmap_save_subset",signature  = c(con = "rmapConnection",  dst = "character"),
definition = function(con, dst, ...) {

  if( !exists_in_master(con,'canvas_ranges') ) { 
      stop ('This project is not prepared. Did you run rmap_prepare()?')
      }

  dbExecute(con, glue("DROP VIEW IF EXISTS {dst}") )

  master = dbReadTable(con, 'rmap_master')

  W = .get_ellipsis(...)

  tnams = W$name
  if( ! all(tnams  %in% master$name) ) stop(
    glue('Invalid table names: {dQuote(tnams[! tnams  %in% master$name]) %>% paste(collapse = ',')}')
    )


  W = merge(master, W, by = 'name')  %>% setDT


  W[type =='map', joinCall := paste('JOIN', name, 'ON', 'c.cell_id = ', name, '.cell_id', collapse =  ''), by = pk]
  W[type =='bio', joinCall := paste('JOIN', name, 'ON', 'c.bio_id = ', name, '.bio_id', collapse =  ''), by = pk]

  if(any(table(W$name)>1)) 
    stop('Table(s) referenced multiple times. USE SQL constructs like "var x = v1 AND var z = v2" ')

  SQL = 
  glue('
    CREATE VIEW IF NOT EXISTS {dst} AS
      SELECT c.cell_id, c.bio_id from canvas_ranges c { paste(W$joinCall, collapse = " ") }
        WHERE { paste(W$whereCall, collapse = " AND ") }
    ')

  dbExecute(con, SQL)  

  write_master(con, 'subset', dst, 'rangeMapper::rmap_save_subset()')


   }
  )
