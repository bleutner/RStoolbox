#' Multi-layer Pixel Entropy
#' 
#' Shannon entropy is calculated for each pixel based on it's layer values.
#' To be used with categorical / integer valued rasters.
#' 
#' Entropy is calculated as -sum(p log(p)); p being the class frequency per pixel.
#' 
#' @param img RasterStack or RasterBrick
#' @param ... additional arguments passed to writeRaster
#' @return
#' RasterLayer "entropy"
#' @export 
#' @examples 
#' data(rlogo)
#' re <- rasterEntropy(rlogo)
#' ggR(re, geom_raster = TRUE)
rasterEntropy <- function(img, ...){
    if(nlayers(img) <= 1) stop("img must have at least two layers")
    out <- calc(img, fun = entropyCpp, forcefun = TRUE, ...)
    out <- .updateLayerNames(out, "entropy")
    out
}


