#' Normalize Raster Images: Center and Scale
#' 
#' For each pixel subtracts the mean of the raster layer and optionally divide by its standard deviation. 
#' 
#' @param img Raster* object. Image to transform. Transformation will be performed separately for each layer.
#' @param norm Logical. Perform normalization (scaling) in addition to centering, i.e. divide by standard deviation.
#' @param ... further arguments passed to \link[raster]{writeRaster}.
#' @return 
#' Returns a Raster* with the same number layers as input layers with each layer being centered and optionally normalized.
#' @export 
#' @examples
#' library(raster)
#' ## Load example data
#' data(rlogo)
#' 
#' ## Normalization: Center and Scale
#' rlogo_center_norm <- normImage(rlogo)
#' hist(rlogo_center_norm)
#' 
#' ## Centering
#' rlogo_center <- normImage(rlogo, norm = FALSE)
normImage <- function(img, norm = TRUE, ...) {
    if(canProcessInMemory(img)) {
        out   <- img
        out[] <- scale(img[], center = TRUE, scale = norm)     
        if("filename" %in% names(list(...))) writeRaster(out, ...)
    } else {    
        means <- cellStats(img, "mean")   
        sds   <- if(norm) cellStats(img, "sd") else rep(1, nlayers(img))
        sds[sds == 0] <- 1
        if(nlayers(img) == 1) {
            out <- calc(img, function(x) {(x - means)/sds}, forcefun = TRUE, ...) 
        } else {
            out <- calc(img, function(x) normImageCpp(x, M = means, S = sds), forcefun = TRUE, ...)
        }
    }
    return(out)
}

