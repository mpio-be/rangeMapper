

context("process Ranges")

test_that("reprojecting on the fly", {

	dbcon = rangeMap.start(file = "wrens.sqlite", dir = tempdir(), overwrite = TRUE)
	f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
	r = readOGR(f, "wrens", verbose = FALSE)
	global.bbox.save(con = dbcon, bbox = f, p4s = CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
	gridSize.save(dbcon, gridSize = 1000000)
	canvas.save(dbcon)

	expect_warning( # because of re-projecting
		processRanges(con = dbcon, spdf = r, ID = "sci_name")  )

	})

test_that("processRanges works with one SpPolyDF without metadata", {

	dbcon = rangeMap.start(file = "wrens.sqlite", dir = tempdir(), overwrite = TRUE)
	f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
	r = readOGR(f, "wrens", verbose = FALSE)
	global.bbox.save(con = dbcon, bbox = r)
	gridSize.save(dbcon, gridSize = 10)
	canvas.save(dbcon)

	processRanges(con = dbcon, spdf = r, ID = "sci_name")

	expect_true( rangeMap.save(dbcon) )

	expect_that(rangeMap.fetch(dbcon) , is_a("SpatialPixelsRangeMap") )

	expect_that(rangeMap.fetch(dbcon, spatial = FALSE) , is_a("data.table") )


	})

test_that("processRanges works with one SpPolyDF with metadata", {

	dbcon = rangeMap.start(file = "wrens.sqlite", dir = tempdir(), overwrite = TRUE)
	f = system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined")
	r = readOGR(f, "wrens", verbose = FALSE)
	global.bbox.save(con = dbcon, bbox = r)
	gridSize.save(dbcon, gridSize = 10)
	canvas.save(dbcon)

	processRanges(con = dbcon, spdf = r, ID = "sci_name", metadata = rangeTraits() )

	expect_true( rangeMap.save(dbcon) )

	expect_that(rangeMap.fetch(dbcon) , is_a("SpatialPixelsRangeMap") )

	expect_that(rangeMap.fetch(dbcon, spatial = FALSE) , is_a("data.table") )

	expect_more_than(nrow( dbGetQuery(dbcon, 'SELECT * from metadata_ranges') ), 0 )

	})

