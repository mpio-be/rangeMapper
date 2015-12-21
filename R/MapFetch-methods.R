setGeneric("rangeMapFetchRaw", function(object, ...) 	standardGeneric("rangeMapFetchRaw") )
setGeneric("rangeMapFetch", function(object, ...) 		standardGeneric("rangeMapFetch") )

setMethod("rangeMapFetch",
	signature  = "rangeMapFetch",
		definition = function(object) {

    	#build tableName(s)
		mapNam = paste(object@MAP, object@tableName, sep = "")

		# map variable
		mapvar = sapply(mapNam, function(x)
					setdiff(dbGetQuery(object@CON, paste("pragma table_info(", x, ")"))$name, object@ID ) )
		# sql string
		dotid = paste('x', 1:length(mapNam), sep = "")
		mapdat = paste(paste(paste(dotid,mapvar, sep = "."), object@tableName, sep = " as "), collapse = ",")

		sql = paste("SELECT c.x, c.y,", mapdat,
		"from canvas as c LEFT JOIN",paste(paste(mapNam, dotid, "on c.id = ", dotid, ".id"), collapse = " LEFT JOIN "))

		map = dbGetQuery(object@CON, sql)

		attr(map, 'p4s') = dbReadTable(object@CON, object@PROJ4STRING)[1,1]

		map

		}
	)

# user level functions

#' rangeMap.fetch

#' @param con 	  a connection to a valid rangeMapper project.
#' @param maps    map(s) name as character vector. If missing then all the maps are returned.
#' @param spatial If TRUE (default) a SpatialPixelsRangeMap is returned, else a data.table.
#' @param bioid   the name of the range to fetch (e.g. species name).
#'
#' @return        an object of SpatialPixelsRangeMap or data.table  containing the spatial coordinates
#'                and proj4 string as an atribute if spatial = FALSE.
#' @export
rangeMap.fetch <- function(con, maps, spatial = TRUE) {
	if(missing(maps)) maps = dbGetQuery(con, 'select name from sqlite_master where type = "table" and tbl_name like "MAP_%"')$name

	maps = gsub("MAP_", "", maps)

	x = new("rangeMapFetch", CON = con, tableName = maps)

	map = rangeMapFetch(x)
	p4s = attr(map, 'p4s')

	if(spatial) {
		coordinates(map) = ~ x + y
		proj4string(map) = CRS(p4s)
		gridded(map) = TRUE
		map = new("SpatialPixelsRangeMap", map, mapvar = maps)
		return(map)
		}

	if(!spatial) {
	return(data.table(map))
	}

  }

#' rangeFetch
#' Range extractor
#'
#' Fetch an arbitrary range from a rangeMapper project.
#'
#' @param rangeMap A \code{\link{rangeMap}} object.
#' @param bioid    A character vector, usually a taxon name, which identifies a
#'                 range within a given rangeMapper project.
#' @return         A \code{\link{SpatialPolygons}}.
#' @export
#' @examples
#'
#' wd = setwd(tempdir())
#' require(rangeMapper)
#' spdf = readOGR(system.file(package = "rangeMapper", "extdata",
#' 	"wrens", "vector_combined"), "wrens", verbose = FALSE)
#' dbcon = rangeMap.start(file = "wrens.sqlite",
#' 	overwrite = TRUE, dir = tempdir() )
#' rmo = rangeMap("wrens.sqlite")
#' global.bbox.save(con = dbcon, bbox = spdf)
#' gridSize.save(dbcon, gridSize = 3)
#' canvas.save(dbcon)
#' processRanges(spdf = spdf, con =  dbcon, ID = "sci_name" )
#' rangeMap.save(dbcon)
#'
#' house_wren = rangeFetch(rmo, "Troglodytes_aedon")
#' image(rangeMap.fetch(dbcon))
#' plot(house_wren, add = TRUE, border = 'blue', lwd = 2)
#' setwd(wd)
#'
rangeFetch <- function(rangeMap, bioid) {
	if( nrow(dbGetQuery(rangeMap@CON, paste("SELECT * from canvas limit 1"))) == 0)
		stop('Empty project!')

	p4s = CRS(dbReadTable(rangeMap@CON, rangeMap@PROJ4STRING)[1,1]) 	# proj4string
	cs2 = dbGetQuery(rangeMap@CON, "select * from gridsize")[1,1]/2 		# 1/2 grid size

	d = dbGetQuery(rangeMap@CON, paste("SELECT c.id, c.x, c.y from canvas c join ranges r on c.id = r.id where r.bioid = ", shQuote(bioid) ) )
	if(nrow(d) == 0)
		stop(paste(dQuote(bioid), 'is not a valid name!'))

	d = split(d, d$id)

	d = lapply(d, function(z) {
		xi = z$x
		yi = z$y
		x = c(xi-cs2, xi-cs2, xi+cs2, xi+cs2, xi-cs2)
		y = c(yi-cs2, yi+cs2, yi+cs2, yi-cs2, yi-cs2)
		Polygons(list(Polygon(coords=cbind(x, y) )), ID = z$id)
		})

	res = SpatialPolygons(d, proj4string= p4s)

	res = gUnionCascaded(res)

	res }




