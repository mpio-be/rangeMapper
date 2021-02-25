#' Wrens Life history.
#' 
#' Life history data of 84 wren species. 
#' 
#' @format A GeoJSON file with with 84 entries and 12 variables. The variables are as follows:
#' \itemize{
#'   \item ID. Entry order as in ref. 1
#'   \item sci_name. Scientific name, character vector
#'   \item com_mame. English name, character vector
#'   \item subspecies. How many subspecies a species has.
#'   \item clutch_size. Mean or modal clutch size
#'   \item male_wing. Male wing length (mm)
#'   \item female_wing. Female wing length (mm)
#'   \item male_tarsus. Male tarsus length (mm)
#'   \item female_tarsus. Female tarsus length (mm)
#'   \item body_mass. Body mass (grams)
#'   \item data_src. bibliographic source of each trait given in the order they appear (see references)
#'   \item geometry.  \link[sf]{sfc} simple feature geometry. 
#' 
#'   } 
#' 
#' @references 
#' \strong{BREEDING RANGES} Ridgely, R.S., T. F. Allnutt, T. Brooks, D. K.
#' McNicol, D. W. Mehlman, B. E. & Young, a.J.R.Z. (2007) Digital Distribution
#' Maps of the Birds of the Western Hemisphere, version 3.0. NatureServe,
#' Arlington, Virginia, USA. \cr \cr
#' \strong{1.}  Brewer, David. Wrens, dippers and thrashers. Bloomsbury Publishing, 2010.  \cr
#' \strong{2.} Kroodsma, D. E., and D. Brewer. "Family Troglodytidae (Wrens)." Lynx Edicions, Barcelona (2005). \cr 
#' \strong{3.} Dunning Jr, John B. CRC handbook of avian body masses. CRC press, 2007.  

#' @keywords datasets
#' @name wrens
#' @examples
#'
#' require(rangeMapper)
#' require(sf)
#' wrens = system.file('extdata','wrens.GeoJSON',package = 'rangeMapper') %>% st_read
#' 
#' # or simpler
#' wrens = read_wrens()
#' 
#' plot(male_wing ~ female_wing, wrens)
#' plot(sf::st_geometry(wrens))
#'
NULL



#' @rdname wrens
#' @note The function read_wrens() reads the 'wrens.GeoJSON' data as a projected sf object.
#' @export
read_wrens <- function() {
    system.file('extdata','wrens.GeoJSON',package = 'rangeMapper') %>%
    st_read(quiet = TRUE)  %>% 
    st_transform("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs") 
    }




#' dem
#' 
#' Digital elevation model of the Americas based on ETOPO1.
#' 
#' 
#' @references 
#' Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global Relief Model: Procedures, Data Sources and Analysis. 
#' NOAA Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009. 
#' Go to this web site: http://www.ngdc.noaa.gov/mgg/global/global.html.

#' @docType data
#' @keywords datasets
#' @name dem
#' @usage data(dem)
#' @format A RasterLayer with 159 rows and 212 columns.
#' @examples
#'
#' require(rangeMapper)
#' data(dem)
#' raster::plot(dem)
#'
NULL