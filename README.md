<!-- README.md is generated from README.Rmd. Please edit that file
knitr::knit('README.Rmd')
-->



[![Travis-CI Build Status](https://travis-ci.org/valcu/rangeMapper.svg?branch=master)](https://travis-ci.org/valcu/rangeMapper)
[![Coverage Status](https://img.shields.io/codecov/c/github/valcu/rangeMapper/master.svg)](https://codecov.io/github/valcu/rangeMapper?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/rangeMapper)](http://cran.r-project.org/package=rangeMapper)


[_rangeMapper_](http://onlinelibrary.wiley.com/doi/10.1111/j.1466-8238.2011.00739.x/full/) is a framework for the study of macroecological patterns of life-history traits.

### _rangeMapper_ in a nutshell


* Datasets (i.e. maps) are retrieved from `sqlite` project files as objects inheriting from `SpatialPixels` or `data.table`. Maps can be plotted directly with `plot()`.

```r
    map = rangeMap.fetch(con, c('median_body_mass', 'median_clutch_size'), spatial = FALSE)
    plot(map, boundary = wrens_boundary, ncol = 2)
```

![](README-1-1.png) 

*  The link between the assemblage level (i.e. the raster cell) and the species level (i.e. the data behind the raster cell) is kept explicit at all stages of the project.
`MAP`s are constructed based on `SQL` aggregate functions or statistical models build in R and can be based on arbitrary subsets defined at both species and assemblage levels.

```R
rangeMap.save(con, FUN = lmSlope, biotab = "life_history_traits",
    biotrait = "body_mass", tableName = "slope_bm_cs",
    formula = log(body_mass) ~ clutch_size,
    subset = list(MAP_species_richness = "SR >= 5",
                  MAP_altitude = "altitude > 1000",
                  BIO_biotab = "Family = 'Troglodytidae'
                                 AND clutch_size is not NULL") )
```


### Installation
```R
devtools::install_github("valcu/rangeMapper") # development.
install.packages("rangeMapper") # stable.
```




