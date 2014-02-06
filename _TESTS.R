
# rangeMap.save() import raster----

## example
dbcon = rangeMap.start(file = "wrens.sqlite", dir = tempdir(), overwrite = TRUE)
f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
global.bbox.save(con = dbcon, bbox = f, p4s = CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
gridSize.save(dbcon, gridSize = 250000)
canvas.save(dbcon)

r = system.file(package = "rangeMapper", "extdata", "etopo1", "etopo1_Americas.tif")
rangeMap.save(dbcon, path = r, tableName = "meanAltitude", FUN = mean, overwrite = TRUE)

m = rangeMap.fetch(dbcon)
plot(m)

rm.rangeMapper(dbcon)

## debug
object = new("MapImport", CON = dbcon, path = r)

# rm.rangeMapper ----
## example
require(rangeMapper)
dbcon = rangeMap.start(file = "wrens.sqlite", dir = tempdir(), overwrite = TRUE)
loc = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
global.bbox.save(con = dbcon, bbox = loc)
gridSize.save(dbcon, gridSize = 4)
canvas.save(dbcon)
r = readOGR(loc, "wrens", verbose = FALSE)
processRanges(spdf = r, con = dbcon, ID = "sci_name")
rangeMap.save(dbcon)
sr = rangeMap.fetch(dbcon)
plot(sr)
rm.rangeMapper(dbcon)
rm.rangeMapper(dbcon, tableName = "MAP_species_richness")
# debug
object =  new("rangeMapRemove", CON = dbcon, tableName = "MAP_species_richness")

object =  new("rangeMapRemove", CON = dbcon)





