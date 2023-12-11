#' Fortify method for classes from the terra package.
#'
#' @param x \code{SpatRaster} object to convert into a dataframe.
#' @param maxpixels Integer. Maximum number of pixels to sample
#' @rdname fortifySpatRaster
#' @usage fortifySpatRaster(x, maxpixels = 50000)
#' @return Returns a data.frame with coordinates (x,y) and corresponding raster values.
#' @name fortifySpatRaster
#' @examples
#' r_df <- fortifySpatRaster(rlogo_rs)
#' head(r_df)
#' @export
fortifySpatRaster <- function(x, maxpixels = 50000){
    raster <- spatSample(x, maxpixels,  method = "regular", as.raster = TRUE)
    as.data.frame(raster, xy = TRUE)
}
