

## examples ----
	require(rangeMapper)
	dbcon = rangeMap.start(file = "wrens.sqlite", dir = tempdir(), overwrite = TRUE)
	f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
	global.bbox.save(con = dbcon, bbox = f, p4s = CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
	gridSize.save(dbcon, gridSize = 250000)
	canvas.save(dbcon)

	r = readOGR(f, "wrens", verbose = FALSE)
	processRanges(spdf = r, con = dbcon, ID = "sci_name", parallel = TRUE)
	processRanges(spdf = r, con = dbcon, ID = "sci_name", parallel = TRUE, metadata = rangeTraits())

	#richness
	m = rangeMap.save(dbcon)
	m = rangeMap.fetch(dbcon)
	plot(m)


	# raster
	r = system.file(package = "rangeMapper", "extdata", "etopo1", "etopo1_Americas.tif")
	rangeMap.save(dbcon, path = r, tableName = "meanAltitude", FUN = mean, overwrite = TRUE)

	m = rangeMap.fetch(dbcon)
	plot(m)

	m = rangeMap.fetch(dbcon, spatial = FALSE)


	rm.rangeMapper(dbcon)


## debug  ----
	object =  new("rangeMapStart", dir = tempdir(), file = 'test4.sqlite')

	object =  new("rangeMap", CON = dbcon)
	object =  new("rangeMapProcess", new("rangeMap", CON = dbcon) )
	spdf = r
	ID = "sci_name"
	metadata = rangeTraits()


	rangeMapProcess(object, spdf, dir, ID, metadata, parallel)



	object =  new("rangeMapFetch", CON = dbcon, tableName = 'species_richness')