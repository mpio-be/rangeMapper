

## examples ----
	dbcon = rangeMap.start(file = "wrens.sqlite", dir = tempdir(), overwrite = TRUE)
	f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
	global.bbox.save(con = dbcon, bbox = f, p4s = CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
	gridSize.save(dbcon, gridSize = 250000)
	canvas.save(dbcon)
	
	r = readOGR(f, "wrens", verbose = FALSE)
	processRanges(spdf = r, con = dbcon, ID = "sci_name")

	#richness
	m = rangeMap.save(dbcon)
	m = rangeMap.fetch(dbcon)
	plot(m)


	# raster
	r = system.file(package = "rangeMapper", "extdata", "etopo1", "etopo1_Americas.tif")
	rangeMap.save(dbcon, path = r, tableName = "meanAltitude", FUN = mean, overwrite = TRUE)

	m = rangeMap.fetch(dbcon)
	plot(m)

	rm.rangeMapper(dbcon)


## debug  ----
	object =  new("rangeMapStart", dir = tempdir(), file = 'test4.sqlite')


