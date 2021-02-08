
context(" -> Wrens <- ")

    test_that("wrens is in place and well defined. ", {
      data(wrens)
      expect_equal(nrow(wrens), 84)
      expect_equal(ncol(wrens), 12)
      expect_s3_class(wrens, 'sf')
        })

context(" -> Connection <- ")

    test_that("rmapConnection can be opened and closed.", {


        # in memory db
        con = rmap_connect()
        expect_is(con, 'rmapConnection')
        expect_true(validObject(con))
        
        dbDisconnect(con)


        # file db
        tf = tempfile()
        con = rmap_connect(tf)
        expect_true(validObject(con))     
        dbDisconnect(con)
        
        # file db & overwrite
        tf = tempfile()
        con = rmap_connect(tf)     
        dbDisconnect(con)

        con = rmap_connect(tf, overwrite = TRUE)   
        expect_true(validObject(con))
        dbDisconnect(con)    
        })

    test_that("rmapConnection fail when db does not have the expected structure.", {

        tf = tempfile()
        con = rmap_connect(tf)     
        dbExecute(con, 'DROP table wkt_ranges')
        dbDisconnect(con)

        expect_error(rmap_connect(tf) )
        })

context(" -> Utilities <- ")

    test_that("is_empty works", {

        con = rmap_connect()     
        expect_true( is_empty(con, 'wkt_ranges') )
        dbDisconnect(con)
        })

    test_that("IO sqlite preserves wkt", {

        con = rmap_connect()
        data(wrens)
        rmap_add_ranges(con, wrens, 'sci_name')
        x = rmap_to_sf(con, 'wkt_ranges')
        dbDisconnect(con)

        expect_identical( sf::st_geometry(wrens)[[1]], sf::st_geometry(x)[[1]] )
        
        expect_equal( st_crs(wrens) , st_crs(x) )
        

        })

    test_that("rmap_to_sf returns sf ", {

        con = rmap_connect()
        rmap_add_ranges(con, wrens, 'sci_name')
        rmap_prepare(con, 'hex', 500)


        expect_is(rmap_to_sf(con, 'wkt_ranges'), 'sf')
        expect_is(rmap_to_sf(con, 'wkt_canvas'), 'sf')
        expect_is(rmap_to_sf(con, 'bbox'),   'sf')

        dbDisconnect(con)

     

        })










