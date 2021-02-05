con = rmap_connect(); on.exit(dbDisconnect(con))
rmap_add_ranges(con, x = wrens, ID = 'sci_name')
rmap_prepare(con, 'hex', cellsize=1000, chunksize = 1)
rmap_save_map(con) 
rmap_save_subset(con, dst = 'ss1', species_richness = 'species_richness > 10')
rmap_save_map(con, subset = 'ss1', dst = 'species_richness_min5')


x = rmap_to_sf(con)
x = rmap_to_sf(con, 'species_richness_min5')

context(" -> Get: basics <- ")
    test_that("rmap_to_sf error when no maps present", {
      conx = rmap_connect(); on.exit(dbDisconnect(conx))
      expect_error(
        rmap_to_sf(conx)
        )
      })  

context(" -> Get: wkt and canvas <- ")
    test_that("rmap_to_sf gets rmap reserved tables as sf", {

      expect_is( rmap_to_sf(con, 'wkt_ranges'), 'sf')
      expect_is( rmap_to_sf(con, 'wkt_canvas'), 'sf')
      expect_is( rmap_to_sf(con, 'bbox'), 'sf')
     
      }) 


context(" -> Get: all maps <- ")
    test_that("rmap_to_sf gets all maps when src is missing", {
      m = rmap_to_sf(con)  
      expect_is(m , 'sf')
      expect_equal(ncol(m), 4)
     
      }) 

context(" -> Get: specific maps <- ")
    test_that("rmap_to_sf gets all maps when src is missing", {
      m = rmap_to_sf(con, 'species_richness')  
      expect_is(m , 'sf')
    
      }) 


context(" -> Get: maps based on a pattern <- ")
    test_that("rmap_to_sf gets all maps when src is missing", {
      m = rmap_to_sf(con, pattern = '^sp')  
      expect_is(m , 'sf')
    
      }) 




