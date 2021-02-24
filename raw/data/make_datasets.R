

# packages, settings
  sapply(c('data.table', 'magrittr','stringr','here','glue',
  'ggplot2', 'sf', 'openxlsx', 'dplyr', 'raster', 'rnaturalearth'), 
  require, character.only = TRUE, quietly = TRUE)

  CRS = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs"

# wrens
  x = read.xlsx('./raw/data/wrens.xlsx') %>% setDT

  s = st_read('./raw/data/wrens.shp')  
  s = dplyr::select(s, -c(com_name, data_src))

  s = st_transform(s, CRS)

  s = merge(x, s, by = 'sci_name', sort = FALSE)
  setcolorder(s, 'ID')

  wrens = st_as_sf(s)

  # export to ./data
  usethis::use_data(wrens, overwrite = TRUE)

 # DEM
  # Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global Relief Model: Procedures, Data Sources and Analysis. NOAA Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009. Go to this web site: http://www.ngdc.noaa.gov/mgg/global/global.html.
  w = 'https://www.ngdc.noaa.gov/mgg/global/relief/ETOPO1/data/bedrock/grid_registered/georeferenced_tiff/ETOPO1_Bed_g_geotiff.zip'
  glue("cd /tmp && wget {w}")  %>% system
  glue("cd /tmp && unzip ETOPO1_Bed_g_geotiff.zip")  %>% system

  r = raster('/tmp/ETOPO1_Bed_g_geotiff.tif')

  x = ne_countries(, returnclass = 'sf')
  x = x[x$region_wb  %in% c('Latin America & Caribbean', 'North America'),]
  x = x[, !x$subunit  %in% c('The Bahamas',  'Trinidad and Tobago', 'Falkland Islands'), ]
  x = st_as_sf(x) %>% st_union

  rs =  crop(r, as(x, 'Spatial') )
  rs= calc(rs, function(x) { x[x<0] <- NA; return(x) } )
  rs = aggregate(rs, fact = 50)
  projection(rs) = '+proj=longlat'
  dem = projectRaster(rs, crs = CRS)

  # export to ./data
  usethis::use_data(dem, overwrite = TRUE)

 

