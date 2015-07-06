#' Center and/or Normalize Raster Images
#' 
#' Normalize / Center an image or image stack.
#' 
#' @param img Raster*. Image to transform. Transformation will be performed separately for each layer.
#' @param norm Logical. Perform normalization in addition to centering.
#' @param ... further arguments passed to writeRaster.
#' @return Returns a Raster* with the same number layers as input layers with each layer being centered and optionally normalized.
#' @export 
#' @examples
#' data(rlogo)
#' # Normalization
#' normImage(rlogo)
#' # Centering
#' normImage(rlogo, norm = FALSE)
normImage <- function(img, norm = TRUE, ...) {
    if(canProcessInMemory(img)) {
        out <- img
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

