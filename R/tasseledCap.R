#' Tasseled Cap Transformation
#' 
#' Calculates brightness, greenness and wetness from multispectral imagery.
#' Currently implemented Landsat 4 TM, Landsat 5 TM, Landsat 7ETM+, Landsat 8 OLI and MODIS.
#' 
#' @param img RasterBrick or RasterStack. Input image. Band order must correspond to sensor specifications (see Details and Examples)
#' @param sat Character. Sensor; one of: c("Landsat4TM", "Landsat5TM", "Landsat7ETM", "Landsat8OLI", "MODIS"). Case is irrelevant.
#' @param ... Further arguments passed to writeRaster.
#' @export 
#' @details 
#' Currently implemented: Landsat 4 TM, Landsat 5 TM, Landsat 7ETM+, Landsat 8 OLI and MODIS. Input data must be in top of atmosphere reflectance.
#' Bands must be available in the correct order and irrelevant bands, such as Landsat Thermal Bands must be removed.
#' Required bands are:
#' \tabular{rrr}{
#'  sat \tab bands \tab coefficients \cr
#'  Landsat4TM \tab 1,2,3,4,5,7 \tab Crist 1985 \cr
#'  Landsat5TM  \tab 1,2,3,4,5,7 \tab Crist 1985 \cr
#'  Landsat7ETM \tab 1,2,3,4,5,7 \tab Huang 2002 \cr
#'  Landsat8OLI \tab 2,3,4,5,6,7 \tab Baig 2014 \cr
#'  MODIS \tab 1,2,3,4,5,6,7 \tab Lobser 2007 \cr
#' }
#' @return 
#' Returns a RasterBrick with the thee bands: brigthness, greenness, and (soil) wetness. 
#' @examples 
#' library(raster)
#' data(lsat)
#' 
#' ## Run tasseled cap (exclude thermal band 6)
#' lsat_tc <- tasseledCap(lsat[[c(1:5,7)]], sat = "Landsat5TM")
#' lsat_tc
#' plot(lsat_tc)
tasseledCap <- function(img, sat, ...) {
    sat <- tolower(sat)
    if(!sat %in% c("landsat4tm" , "landsat5tm" , "landsat7etm" ,"landsat8oli", "modis")) stop("Sensor not implemented. See ?tasseledCap for options.")     
    
    
    d <- list(NULL, c("brightness", "greenness", "wetness"))
    coefs <- list(
            landsat4tm = matrix(c(
                            # Crist 1985
                             0.2043,  0.4158,  0.5524, 0.5741,  0.3124,  0.2303, 
                            -0.1063, -0.2819, -0.4934, 0.7940, -0.0002, -0.1446,
                             0.0315,  0.2021,  0.3102, 0.1594, -0.6806, -0.6109
    ), ncol=3, dimnames = d),
            landsat5tm = matrix( c(
                            # Crist 1985
                            0.2043,  0.4158,  0.5524, 0.5741,  0.3124,  0.2303, 
                            0.1063, -0.2819, -0.4934, 0.7940, -0.0002, -0.1446,
                            0.0315,  0.2021,  0.3102, 0.1594, -0.6806, -0.6109)
                    , ncol = 3, dimnames = d),
            landsat7etm= matrix(c( 
                            # Huang 2002
                             0.3561,  0.3972, 0.3904, 0.6966, 0.2286, 0.1596, 
                            -0.3344, -0.3544,-0.4556, 0.6966,-0.0242,-0.2630,
                             0.2626,  0.2141, 0.0926, 0.0656,-0.7629,-0.5388)
                    #       0.0805, -0.0498 ,0.1950,-0.1327, 0.5752,-0.7775, 
                    #      -0.7252, -0.0202, 0.6683, 0.0631,-0.1494,-0.0274, 
                    #       0.4000, -0.8172, 0.3832, 0.0602,-0.1095, 0.0985 
                    , ncol = 3, dimnames = d),          
            landsat8oli= matrix(c(
                            # Baig et al (2014)
                            0.3029, 0.2786, 0.4733, 0.5599, 0.5080, 0.1872,
                            -0.2941,-0.2430,-0.5424, 0.7276, 0.0713,-0.1608,
                            0.1511, 0.1973, 0.3283, 0.3407,-0.7117,-0.4559), ncol = 3, dimnames = d),    
            modis = matrix(c(
                            #Lobser & Cohen (2007) 
                            0.4395, 0.5945, 0.2460, 0.3918, 0.3506, 0.2136, 0.2678, 
                            -0.4064, 0.5129,-0.2744,-0.2893, 0.4882,-0.0036,-0.4169,
                            0.1147, 0.2489, 0.2408, 0.3132,-0.3122,-0.6416,-0.5087), ncol = 3, dimnames = d)
    #ikonos = matrix(c(NA)),
    #spot = matrix(c(NA)),
    #rapideye = matrix(c(NA)),
    )
    if(nlayers(img) != nrow(coefs[[sat]])) stop("Number of layers does not match required number of layers")
    
    tct <- function(x, cof = coefs[[sat]]) {
        x %*% cof 
    }
    
    calc(img, fun = tct, forcefun = TRUE, ...)
}




