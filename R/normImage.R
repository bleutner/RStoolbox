#' Normalize Raster Images: Center and Scale
#' 
#' For each pixel subtracts the mean of the raster layer and optionally divide by its standard deviation. 
#' 
#' @param img  SpatRaster. Image to transform. Transformation will be performed separately for each layer.
#' @param norm Logical. Perform normalization (scaling) in addition to centering, i.e. divide by standard deviation.
#' @param ... further arguments passed to \link[terra]{writeRaster}.
#' @return 
#' Returns a SpatRaster with the same number layers as input layers with each layer being centered and optionally normalized.
#' @export 
#' @examples
#' library(terra)
#' ## Load example data
#' 
#' ## Normalization: Center and Scale
#' rlogo_center_norm <- normImage(rlogo_rs)
#' hist(rlogo_center_norm)
#' 
#' ## Centering
#' rlogo_center <- normImage(rlogo_rs, norm = FALSE)
normImage <- function(img, norm = TRUE, ...) {
  img <- .toTerra(img)

  if(inherits(img, "SpatRaster")) {
    out <- scale(img, TRUE, norm)
    if("filename" %in% names(list(...))) out <- terra::writeRaster(out, ...)
  } else if(.canProcInMem(img)) {
    out   <- img
    out[] <- scale(img[], center = TRUE, scale = norm)
    if("filename" %in% names(list(...))) out <- terra::writeRaster(out, ...)
  } else {
    means <- as.numeric(t(terra::global(img, "mean")))
    names(means) <- names(img)
    sds <- if(norm){
      as.numeric(t(terra::global(img, "mean")))
      names(means) <- names(img)
    }else {
      rep(1, nlyr(img))
    }

    stop()

    sds[sds == 0] <- 1
    if(nlyr(img) == 1) {
      out <- app(img, function(x) {(x - means)/sds}, ...)
    } else {
      out <- app(img, function(x) normImageCpp(x, M = means, S = sds), ...)
    }
  }
  return(out) 
}
