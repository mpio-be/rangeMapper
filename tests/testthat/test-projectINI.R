context("low level")


test_that("Range overlay", {
	con = rangeMap.start(file = "wrens.sqlite", dir = tempdir(), overwrite = TRUE)
	spdf = readOGR( system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined"), "wrens", verbose = FALSE)

	global.bbox.save(con = con, bbox = spdf)
	gridSize.save(con, gridSize = 10)
	canvas.save(con)
	canvas = canvas.fetch(con)

	name = 'Campylorhynchus_gularis'
	spp = spdf[spdf$sci_name == name, ]

	expect_that( rangeOverlay(spp, canvas, name) , is_a("data.frame") )

	})

test_that("Canvas", {

	spdf = readOGR( system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined"), "wrens", verbose = FALSE)
	name = 'Campylorhynchus_gularis'
	spp = spdf[spdf$sci_name == name, ]

	con = rangeMap.start(file = "wrens.sqlite", dir = tempdir(), overwrite = TRUE)
	global.bbox.save(con = con, bbox = spdf)
	gridSize.save(con, gridSize = 10)
	canvas.save(con)

	ID = "sci_name"
	metadata = rangeTraits()
	canvas = canvas.fetch(con)

	expect_that(canvas , is_a("SpatialPixelsDataFrame") )

	rmo =  new("rangeMap", CON = con)

	p4s =  dbReadTable(con, "proj4string")[1,1]
	ids = spdf@data[, ID]


	})

