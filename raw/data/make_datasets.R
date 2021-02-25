

# packages, settings
  sapply(c('data.table', 'magrittr','stringr','here','glue',
  'ggplot2', 'sf', 'openxlsx', 'dplyr', 
  'raster', 'rnaturalearth', 'gdalUtils', 'rmapshaper'), 
  require, character.only = TRUE, quietly = TRUE)

# wrens
  x = read.xlsx('./raw/data/wrens.xlsx') %>% setDT

  s = st_read('./raw/data/wrens.shp')  
  s = dplyr::select(s, -c(com_name, data_src))

  s = st_transform(s, '+proj=longlat +datum=WGS84 +no_defs')

  s = merge(x, s, by = 'sci_name', sort = FALSE)
  setcolorder(s, 'ID')

  wrens = st_as_sf(s)

  st_write(s, './inst/extdata/wrens.GeoJSON')

# DEM
  # Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global Relief Model: Procedures, Data Sources and Analysis. NOAA Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009. Go to this web site: http://www.ngdc.noaa.gov/mgg/global/global.html.
  CRS = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs"

  x = ne_countries(, returnclass = 'sf')
  x = x[x$region_wb  %in% c('Latin America & Caribbean', 'North America'),]
  x = x[, !x$subunit  %in% c('The Bahamas',  'Trinidad and Tobago', 'Falkland Islands'), ]
  x = st_as_sf(x) %>% ms_dissolve 
  st_write(x, '/tmp/etopo_roi.shp')

  w = 'https://www.ngdc.noaa.gov/mgg/global/relief/ETOPO1/data/bedrock/grid_registered/georeferenced_tiff/ETOPO1_Bed_g_geotiff.zip'
  glue("cd /tmp && wget {w}")  %>% system
  glue("cd /tmp && unzip ETOPO1_Bed_g_geotiff.zip")  %>% system

  gdalwarp(
        '/tmp/ETOPO1_Bed_g_geotiff.tif',
        dstfile          = '/tmp/ETOPO1_Bed_g_geotiff_w.tif',
        s_srs            = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',
        t_srs            = CRS,
        cutline          =  '/tmp/roi.shp',
        crop_to_cutline  = TRUE, 
        output_Raster    = TRUE,
        verbose          = TRUE)
  system('gdal_calc.py -A /tmp/ETOPO1_Bed_g_geotiff_w.tif --outfile=/tmp/ETOPO1_Bed_g_geotiff_w2.tif --calc="A*(A> 0)" --NoDataValue=0' )

  r = raster('/tmp/ETOPO1_Bed_g_geotiff_w2.tif')
  dem = aggregate(r, fact = 50)
  names(dem) = 'dem'

  # export to ./data
  usethis::use_data(dem, overwrite = TRUE)

 

