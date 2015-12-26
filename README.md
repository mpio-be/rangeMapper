<!-- README.md is generated from README.Rmd. Please edit that file
knitr::knit('README.Rmd')
-->


[![Travis-CI Build Status](https://travis-ci.org/valcu/rangeMapper.svg?branch=master)](https://travis-ci.org/valcu/rangeMapper)
[![Coverage Status](https://img.shields.io/codecov/c/github/valcu/rangeMapper/master.svg)](https://codecov.io/github/valcu/rangeMapper?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/rangeMapper)](http://cran.r-project.org/package=rangeMapper)


```R
rangeMap.save(con, FUN = lmSlope, biotab = "life_history_traits",
    biotrait = "body_mass", tableName = "slope_bm_cs",
    formula = log(body_mass) ~ clutch_size,
    subset = list(MAP_species_richness = "SR >= 5",
                  MAP_altitude = "altitude > 1000",
                  BIO_biotab = "Family = 'Troglodytidae'
                                 AND clutch_size is not NULL) )
```
![](README-1-1.png) 




### Installation
```R
devtools::install_github("valcu/rangeMapper") # developement.
install.packages("rangeMapper") # stable.
```




