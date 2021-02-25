wrens = read_wrens()

context(" -> Add ranges <- ")

    test_that("rmap_add_ranges work", {

        con = rmap_connect()
        expect_true(rmap_add_ranges(con, wrens, 'sci_name') )

        expect_error(rmap_add_ranges(con, wrens, 'sci_name') )

        dbDisconnect(con) 

        })

context(" -> Prepare: un-exported functions <- ")
    
    test_that("rmap_refresh works", {
        con = rmap_connect()
        rmap_add_ranges(con, wrens, 'sci_name') 
        rmap_prepare(con, 'hex', 5000)

        expect_equal(rmap_refresh(con), 0L)

        dbDisconnect(con)    
        
        })

    test_that("make_regular_canvas works", {
        con = rmap_connect()
        rmap_add_ranges(con, wrens, 'sci_name') 

        expect_s3_class( make_regular_canvas(con, 'hex', 5000) , 'sf')
        expect_s3_class( make_regular_canvas(con, 'square', 5000) , 'sf')

        dbDisconnect(con)    
        
        })

context(" -> Prepare <- ")

    test_that("default rmap_prepare work", {
        con = rmap_connect( tempfile() )
        rmap_add_ranges(con, wrens, 'sci_name') 
        expect_true( rmap_prepare(con, 'hex', 5000)  )
        dbDisconnect(con)    
        })

    test_that("rmap_prepare in-memory should not work in parallel", {
        con = rmap_connect()
        rmap_add_ranges(con, wrens, 'sci_name') 

        # 'in-memory' should not support parallel processing 
        expect_error( rmap_prepare(con, 'hex', 5000, chunksize = 1/2)  )
        dbDisconnect(con)    
        })

    test_that("rmap_prepare restart graciously", {
        con = rmap_connect()
        rmap_add_ranges(con, wrens, 'sci_name') 

        rmap_prepare(con, 'hex', 5000)
        expect_message(rmap_prepare(con, 'hex', 5000), regexp = 'overwritten')

        dbDisconnect(con)    
        })

    test_that("options rmap.verbose = TRUE stops messages", {
        
        options(rmap.verbose = FALSE)
        con = rmap_connect()
        rmap_add_ranges(con, wrens, 'sci_name') 
        expect_silent(rmap_prepare(con, 'hex', 5000))
        dbDisconnect(con)   

        options(rmap.verbose = TRUE)


        })

    test_that("parallel rmap_prepare work", {
        
        require(future)
        plan(multisession, workers = 1)


        con = rmap_connect(tempfile() )
        rmap_add_ranges(con, wrens, 'sci_name') 

        # chunksize should in (0,1)
        expect_error(rmap_prepare(con, 'hex', 5000, chunksize =0) )
        expect_error(rmap_prepare(con, 'hex', 5000, chunksize =1) )


        rmap_prepare(con, 'hex', 5000, chunksize = 1/2)
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


