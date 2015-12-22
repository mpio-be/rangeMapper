context("3: Output")

test_that("rangeMapFetch produces a SpatialPixelsRangeMap", {

	dbcon = rangeMap.start(file = "wrens.sqlite", dir = tempdir(), overwrite = TRUE)
	f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
	r = rgdal::readOGR(f, "wrens", verbose = FALSE)[1:10, ]
	global.bbox.save(con = dbcon, bbox = r)
	gridSize.save(dbcon, gridSize = 10)
	canvas.save(dbcon)
	processRanges(con = dbcon, spdf = r, ID = "sci_name")
	rangeMap.save(dbcon)
	m= rangeMap.fetch(dbcon)


	})

test_that("rangeMapExport produces a tiff", {

	dbcon = rangeMap.start(file = "wrens.sqlite", dir = tempdir(), overwrite = TRUE)
	f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
	r = rgdal::readOGR(f, "wrens", verbose = FALSE)[1:10, ]
	global.bbox.save(con = dbcon, bbox = r)
	gridSize.save(dbcon, gridSize = 10)
	canvas.save(dbcon)
	processRanges(con = dbcon, spdf = r, ID = "sci_name")
	rangeMap.save(dbcon)
	m= rangeMap.fetch(dbcon)

	rastPath = paste( rangeMap.export(dbcon, dir = tempdir()), 'MAP_species_richness.tiff', sep = .Platform$file.sep)
	r = rgdal::readGDAL(rastPath, silent = TRUE)

	expect_that(r, is_a('SpatialGridDataFrame'))

	})
