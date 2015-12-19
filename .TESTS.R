

## examples ----
	require(rangeMapper)

	require(doParallel)
	cl = makePSOCKcluster(detectCores())
	registerDoParallel(cl)
	# stopCluster(cl)


	dbcon = rangeMap.start(file = "wrens.sqlite", dir = tempdir(), overwrite = TRUE)
	f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
	global.bbox.save(con = dbcon, bbox = f, p4s = CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
	gridSize.save(dbcon, gridSize = 250000)
	canvas.save(dbcon)

	r = readOGR(f, "wrens", verbose = FALSE)

	# one file, metadata are computed
		processRanges(con = dbcon, spdf = r, ID = "sci_name", metadata = rangeTraits() )
		rangeMap.save(dbcon)
		m = rangeMap.fetch(dbcon)
		rangeMap.fetch(dbcon, spatial = FALSE)
		plot(m)
		rm.rangeMapper(dbcon)

	# one file no metadata
		processRanges(con = dbcon, spdf = r, ID = "sci_name")
		rangeMap.save(dbcon)
		m = rangeMap.fetch(dbcon)
		rangeMap.fetch(dbcon, spatial = FALSE)
		plot(m)
		rm.rangeMapper(dbcon)

	# multiple files no metadata
		rdr = system.file(package = "rangeMapper", "extdata", "wrens", "vector")
		processRanges(dir = rdr, con =  dbcon)

		rangeMap.save(dbcon)
		m = rangeMap.fetch(dbcon)
		rangeMap.fetch(dbcon, spatial = FALSE)
		plot(m)
		rm.rangeMapper(dbcon)

	# multiple files metadata are computed
			rdr = system.file(package = "rangeMapper", "extdata", "wrens", "vector")
			processRanges(dir = rdr, con =  dbcon, metadata = rangeTraits() )

			rangeMap.save(dbcon)
			m = rangeMap.fetch(dbcon)
			rangeMap.fetch(dbcon, spatial = FALSE)
			plot(m)
		rm.rangeMapper(dbcon)


	# rangeMap.save rasters
		r = system.file(package = "rangeMapper", "extdata", "etopo1", "etopo1_Americas.tif")
		rangeMap.save(dbcon, path = r, tableName = "meanAltitude", FUN = mean, overwrite = TRUE)

		m = rangeMap.fetch(dbcon)
		plot(m)




## debug  ----
	object =  new("rangeMapStart", dir = tempdir(), file = 'test4.sqlite')
	object =  new("rangeMapProcess", new("rangeMap", CON = dbcon) )
	object =  new("rangeMapFetch", CON = dbcon, tableName = 'species_richness')

	spdf = r
	dir = rdr
	ID = "sci_name"
	metadata = rangeTraits()
	con = dbcon
	canvas = canvas.fetch(dbcon)
	rmo =  new("rangeMap", CON = con)


