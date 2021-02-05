con = rmap_connect()
on.exit(dbDisconnect(con))
rmap_add_ranges(con, x = wrens, ID = 'sci_name')
rmap_prepare(con, 'hex', cellsize=5000, chunksize = 1)
rmap_add_bio(con, wrens, 'sci_name')
rmap_save_subset(con,'s1', wrens = 'clutch_size > 2')  
rmap_save_map(con)
rmap_save_subset(con,'s2', species_richness = 'species_richness > 5')  


context(" -> Maps: basics <- ")

  test_that("rmap_save_map errors without canvas", {
    conx = rmap_connect(); on.exit(dbDisconnect(conx))
    expect_error(
      rmap_save_map(conx)
      )
    })  

context(" -> Maps: SQL functions <- ")


  test_that("rmap_save_map with no arguments saves species_richnes", {
    expect_true(
      rmap_save_map(con)
      )
    })


  test_that("rmap_save_map works with missing function and subset", {
    expect_true(
      rmap_save_map(con, subset = 's1', dst = 'SR')
      )
    })

  test_that("rmap_save_map works with SQL functions", {
    expect_true(
       rmap_save_map(con, fun='avg', src='wrens',v='body_mass', dst='avg_bodymass')
      )
    })


  test_that("rmap_save_map works with SQL functions and subset", {
    expect_true(
       rmap_save_map(con, fun='avg', src='wrens',v='body_mass', dst='avg_bodymass', subset = 's1')
      )
    })

context(" -> Maps: R functions <- ")

  test_that("rmap_save_map R function- one argument", {
    
    rmap_save_map(con, fun= mean, na.rm = TRUE, src='wrens',v='body_mass', dst='m')
    m = rmap_to_sf(con, 'm')
    
    expect_s3_class(m, 'sf')
    
    })

  test_that("rmap_save_map: custom functions -cor ", {
    
    fun = function(x) { data.frame(cor = cor(x$male_wing, x$male_tarsus) ) }
    rmap_save_map(con, fun= fun, src='wrens', dst='m')
    m = rmap_to_sf(con, 'm')
    expect_s3_class(rmap_to_sf(con, 'm'), 'sf')
    rmap_save_map(con, fun= fun, src='wrens', dst='m', subset ='s2')
    m = rmap_to_sf(con, 'm')
    expect_s3_class(rmap_to_sf(con, 'm'), 'sf')
    
    })

  test_that("rmap_save_map: custom functions - lm ", {
    
    fun = function(x) {
      lm(clutch_size ~ log(female_tarsus), x) %>% 
      summary %>% coefficients %>% data.table %>% .[-1]
      }

    rmap_save_map(con, fun= fun, src='wrens', dst='m')
    
    m = rmap_to_sf(con, 'm')
    expect_s3_class(rmap_to_sf(con, 'm'), 'sf')

    })

  test_that("rmap_save_map: custom functions - nlme", {
    
    fun = function(x) {
      nlme::lme(clutch_size ~ log(female_tarsus), random = ~1|bio_id, x) %>% 
      summary %>% coefficients %>% data.frame %>% .[-1, ]
      }

    rmap_save_map(con, fun= fun, src='wrens', dst='m')
    
    m = rmap_to_sf(con, 'm')
    expect_s3_class(rmap_to_sf(con, 'm'), 'sf')

    })

  test_that("rmap_save_map: custom functions - lm ", {
    
    fun = function(x) {
      lm(clutch_size ~ log(female_tarsus), x) %>% 
      summary %>% coefficients %>% data.table
      }

    expect_warning(
      rmap_save_map(con, fun= fun, src='wrens', dst='m')
      )


    })

context(" -> Raster import <- ")

  test_that("rmap_save_map imports RasterLayer", {
    
    data(dem)
    rmap_save_map(con, fun= 'mean', src= dem ,  dst='dem', progress = FALSE)

    m = rmap_to_sf(con, 'dem')

    expect_s3_class(m, 'sf')

    })
  test_that("rmap_save_map imports RasterBrick", {
    
    data(dem)
    dems = raster::brick(list(dem, dem))

    rmap_save_map(con, fun= 'mean', src= dems ,  dst='dem2', progress = FALSE)


    m = rmap_to_sf(con, 'dem2')

    expect_s3_class(m, 'sf')

    })





