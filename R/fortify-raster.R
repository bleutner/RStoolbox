#' Fortify method for classes from the raster package.
#'
#' @param x \code{Raster*} object to convert into a dataframe.
#' @param maxpixels Integer. Maximum number of pixels to sample
#' @param ... not used by this method
#' @return Returns a data.frame with coordinates (x,y) and corresponding raster values.
#' @name fortify.raster
#' @examples
#' library(ggplot2)
#' data(rlogo)
#' r_df <- fortify(rlogo)
#' head(r_df)
#' 
NULL

#' @rdname fortify.raster
#' @export
#' @method fortify RasterLayer
fortify.RasterLayer <- function(x, maxpixels = 50000){
    
    if(inherits(x, "Raster")) {
      raster <- sampleRegular(x, maxpixels, asRaster = TRUE)
    } else {
      raster <- spatSample(x, maxpixels,  method = "regular", as.raster = TRUE)
    }
    
    
    if(inherits(x, "Raster") && .nlyr(x) == 1 && is.factor(x)) raster <- stack(raster,raster)  ## workaround raster bug #6043
    as.data.frame(raster, xy = TRUE)
        
}


#' @rdname fortify.raster
#' @export
#' @method fortify RasterBrick
fortify.RasterBrick <- function(...){
    fortify.RasterLayer(...)    
}

#' @rdname fortify.raster
#' @export
#' @method fortify RasterStack
fortify.RasterStack <- function(...){
    fortify.RasterLayer(...)    
}

#' @rdname fortify.raster
#' @export
#' @method fortify SpatRaster
fortify.SpatRaster <- function(...){
  fortify.RasterLayer(...)    
}
