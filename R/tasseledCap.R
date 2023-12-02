#' Tasseled Cap Transformation
#' 
#' Calculates brightness, greenness and wetness from multispectral imagery.
#' Currently implemented Landsat 4 TM, Landsat 5 TM, Landsat 7ETM+, Landsat 8 OLI, MODIS, QuickBird, Spot5 and RapidEye.
#' 
#' @param img RasterBrick or RasterStack or SpatRaster. Input image. Band order must correspond to sensor specifications (see Details and Examples)
#' @param sat Character. Sensor; one of: c("Landsat4TM", "Landsat5TM", "Landsat7ETM", "Landsat8OLI", "MODIS", "QuickBird", "Spot5", "RapidEye"). Case is irrelevant.
#' @param ... Further arguments passed to writeRaster.
#' @export 
#' @details 
#' Currently implemented: Landsat 4 TM, Landsat 5 TM, Landsat 7ETM+, Landsat 8 OLI, MODIS, QuickBird, Spot5, RapdiEye.
#' Input data must be in top of atmosphere reflectance.
#' Moreover, bands must be provided in ascending order as listed in the table below. 
#' Irrelevant bands, such as Landsat Thermal Bands or QuickBird/Spot5 Panchromatic Bands must be omitted.
#' Required bands are:
#' \tabular{rrrl}{
#'  sat \tab bands \tab coefficients \tab data unit\cr
#'  Landsat4TM \tab 1,2,3,4,5,7 \tab Crist 1985 \tab reflectance \cr
#'  Landsat5TM  \tab 1,2,3,4,5,7 \tab Crist 1985 \tab reflectance \cr
#'  Landsat7ETM \tab 1,2,3,4,5,7 \tab Huang 2002 \tab reflectance \cr
#'  Landsat8OLI \tab 2,3,4,5,6,7 \tab Baig 2014 \tab reflectance \cr
#'  MODIS \tab 1,2,3,4,5,6,7 \tab Lobser 2007 \tab reflectance \cr
#'  QuickBird \tab 2,3,4,5 \tab Yarbrough 2005 \tab reflectance \cr
#'  Spot5 \tab 2,3,4,5 \tab Ivtis 2008 \tab reflectance \cr
#'  RapidEye \tab  1,2,3,4,5 \tab Schoenert 2014 \tab reflectance \cr
#' }
#' @references 
#' Crist (1985) "A TM Tasseled Cap Equivalent Transformation for Reflectance Factor Data." Remote Sensing of Environment 17 (3): 301-306
#' 
#' Huang et al. (2002) "Derivation of a Tasselled Cap Transformation Based on Landsat 7 At-Satellite Reflectance." International Journal of Remote Sensing 23 (8): 1741-1748
#' 
#' Baig et al. (2014) "Derivation of a Tasselled Cap Transformation Based on Landsat 8 At-Satellite Reflectance." Remote Sensing Letters 5 (5): 423-431.
#' 
#' Lobser et al. (2007) "MODIS Tasselled Cap: Land Cover Characteristics Expressed through Transformed MODIS Data." International Journal of Remote Sensing 28 (22): 5079-5101.
#' 
#' Yarbrough et al. (2005) "QuickBird 2 tasseled cap transform coefficients: a comparison of derivation methods." Pecora 16 Global Priorities in Land Remote Sensing: 23-27.
#' 
#' Ivits et al. (2008) "Orthogonal transformation of segmented SPOT5 images." Photogrammetric Engineering & Remote Sensing 74 (11): 1351-1364.
#' 
#' Schoenert et al. (2014) "Derivation of tasseled cap coefficients for RapidEye data." Earth Resources and Environmental Remote Sensing/GIS Applications V (9245): 92450Qs.
#' @return 
#' Returns a SpatRaster with the thee bands: brigthness, greenness, and (soil) wetness.
#' @examples 
#' library(terra)
#' data(lsat)
#' 
#' ## Run tasseled cap (exclude thermal band 6)
#' lsat_tc <- tasseledCap(lsat[[c(1:5,7)]], sat = "Landsat5TM")
#' lsat_tc
#' plot(lsat_tc)
tasseledCap <- function(img, sat, ...) {
	img <- .toTerra(img)
	
    sat <- tolower(sat)
    if(!sat %in% c("landsat4tm" , "landsat5tm" , "landsat7etm" ,"landsat8oli", "modis", "quickbird", "spot5", "rapideye")) stop("Sensor not implemented. See ?tasseledCap for options.")     
    
    if(nlyr(img) != nrow(.TCcoefs[[sat]])) stop("Number of layers does not match required number of layers")
    
    tct <- function(x, cof = .TCcoefs[[sat]]) {
        x %*% cof 
    }
    
	out <- app(img, fun = tct, ...)
	out <- .updateLayerNames(out, colnames(.TCcoefs[[sat]]))
	out
}



 
 