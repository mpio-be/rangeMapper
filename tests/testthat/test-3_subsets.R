
con = rmap_connect()
on.exit(dbDisconnect(con))
rmap_add_ranges(con, x = wrens, ID = 'sci_name')
rmap_prepare(con, 'hex', cellsize=500)
rmap_add_bio(con, wrens, 'sci_name')
rmap_save_map(con) 



context(" -> Subsets: basics <- ")

  test_that("rmap_save_subset error on up-prepared project", {
    
    conx = rmap_connect(); on.exit(dbDisconnect(conx))
    expect_error(
      rmap_save_subset(conx,'s1', species_richness = 'species_richness > 10')
      )
    })

  test_that("rmap_save_subset returns TRUE", {
    
    expect_true(
      rmap_save_subset(con,'s1', species_richness = 'species_richness > 10')
      )
    })

  test_that("rmap_save_subset error on wrong table name", {
    
    expect_error(
      rmap_save_subset(con,'s1', not_a_table = 'species_richness > 10')
      )
    })

  test_that("rmap_save_subset error on malformed SQL", {
    
    expect_error(
      rmap_save_subset(con,'s1', species_richness = 'species_richness > 00xx')
      )

    })

  test_that("rmap_save_subset allows only one table call", {
    
    expect_error(
      rmap_save_subset(con,'s1', 
          wrens            = 'female_tarsus BETWEEN 20 and 30', 
          wrens            = 'subspecies > 1'
          )
      )
    })


context(" -> Subsets: examples <- ")

  test_that("rmap_save_subset works with LIKE", {
    
    expect_true(
      rmap_save_subset(con,'s1', 
          wrens  = 'female_tarsus BETWEEN 20 and 30 AND subspecies > 1 AND com_name LIKE  "%-wren%" '
          )
      )
    })


  test_that("rmap_save_subset works with where > (select .... ) ", {
    
    expect_true(
      rmap_save_subset(con,'s1', 
          wrens  = 'clutch_size > (SELECT avg(clutch_size) from wrens)'
          )
      )
    })





