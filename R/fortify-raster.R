#' Fortify method for classes from the terra package.
#'
#' @param x \code{Raster*} object to convert into a dataframe.
#' @param maxpixels Integer. Maximum number of pixels to sample
#' @param ... not used by this method
#' @return Returns a data.frame with coordinates (x,y) and corresponding raster values.
#' @name fortify.Spatraster
#' @examples
#' library(ggplot2)
#' r_df <- fortify(rlogo)
#' head(r_df)
#' 
NULL

#' @rdname fortify.Spatraster
#' @export
#' @method fortify SpatRaster
fortify.SpatRaster <- function(x, maxpixels = 50000){
    raster <- spatSample(x, maxpixels,  method = "regular", as.raster = TRUE)
    as.data.frame(raster, xy = TRUE)
}
