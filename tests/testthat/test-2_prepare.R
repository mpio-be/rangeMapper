
context(" -> Add ranges <- ")

    test_that("rmap_add_ranges work", {

        con = rmap_connect()
        expect_true(rmap_add_ranges(con, wrens, 'sci_name') )

        expect_error(rmap_add_ranges(con, wrens, 'sci_name') )

        dbDisconnect(con) 

        })

context(" -> Prepare <- ")

    test_that("default rmap_prepare work", {
        con = rmap_connect( tempfile() )
        rmap_add_ranges(con, wrens, 'sci_name') 
        expect_true( rmap_prepare(con, 'hex', 5000)  )
        dbDisconnect(con)    
        })

    test_that("default rmap_prepare work", {
        con = rmap_connect()
        rmap_add_ranges(con, wrens, 'sci_name') 

        # because 'in-memory' does not support parallel processing hence chunksize > 1
        expect_warning( rmap_prepare(con, 'hex', 5000)  )
        dbDisconnect(con)    
        })

    test_that("parallel rmap_prepare work", {
        
        require(future)
        plan(multisession, workers = 1)


        con = rmap_connect(tempfile() )
        rmap_add_ranges(con, wrens, 'sci_name') 

        rmap_prepare(con, 'hex', 5000)
        dbDisconnect(con)    

        })

context(" -> Add bio <- ")

    test_that("BIO tables imports what is should", {

        con = rmap_connect()
        rmap_add_ranges(con, wrens, 'sci_name')
     
        expect_error( rmap_add_bio(con, unclass(wrens)  , 'sci_name') )
     
        dbDisconnect(con) 

        })

    test_that("BIO tables imports after ranges & warns when over-write", {

        con = rmap_connect()
        expect_error( rmap_add_bio(con, wrens, 'sci_name') )

        rmap_add_ranges(con, wrens, 'sci_name')
        
        expect_true( rmap_add_bio(con, wrens, 'sci_name') )
        expect_warning( rmap_add_bio(con, wrens, 'sci_name') )
        

        dbDisconnect(con) 

        })

    test_that("BIO table import checks ID matching", {

        con = rmap_connect()
        rmap_add_ranges(con, wrens, 'sci_name')
        expect_error( rmap_add_bio(con, wrens, 'com_name') )

        x = data.table( st_drop_geometry(wrens) )
        x[1:10, sci_name := toupper(sci_name)]
        expect_warning( rmap_add_bio(con, x, 'sci_name') )
        dbDisconnect(con) 

        })


