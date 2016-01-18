#' Change Vector Analysis
#' 
#' Calculates angle and magnitude of change vectors. 
#' Dimensionality is limited to two bands per image. 
#' 
#' @param x RasterBrick or RasterStack with two layers. Both rasters (y and y) need to correspond to each other, i.e. same resolution, extent and origin.
#' @param y RasterBrick or RasterStack with two layers. Both rasters (y and y) need to correspond to each other, i.e. same resolution, extent and origin.
#' @param ... further arguments passed to writeRaster
#' @details 
#' Change Vector Analysis (CVA) is used to identify spectral changes between two identical scenes which were acquired at different times. 
#' CVA is limited to two bands per image. For each pixel it calculates the change in the two-dimensional spectral space. 
#' For example for a given pixel in image A and B for the red and nir band the change vector is caclulated for the coordinate pairs: (red_A | nir_A) and (red_B | nir_B).
#' 
#' @return 
#' Returns a RasterBrick with two layers: change vector angle and change vector magnitude
#' @export 
#' @examples 
#' library(raster)
#' ## Create example data
#' data(lsat)
#' pca <- rasterPCA(lsat)$map
#' 
#' ## Do change vector analysis 
#' cva <- rasterCVA(pca[[1:2]], pca[[3:4]])
#' cva
#' plot(cva)
rasterCVA <- function(x, y, ...) {
    if(nlayers(x) != 2 | nlayers(y) != 2) stop("need two rasters with two layers each")
    
    anglefun <- function(x, y) {
        magout <- angle <- rep(0, nrow(x))
        dif <- x - y
        magnitude <- sqrt(rowSums(dif^2))
        threshold <- 2 * median(magnitude[magnitude > 0], na.rm = TRUE)
        sel <- magnitude > threshold & !is.na(magnitude)
        if(any(sel)){
            magout[sel] <- magnitude[sel]
            angle[sel]  <- atan(dif[sel,2]/dif[sel,1]) / pi * 180
        }
        angle[is.na(magnitude)]<-NA
        magout[is.na(magnitude)]<-NA
        
        m <- cbind(angle, magout)
    }
    
    map <- overlay(x[[1:2]], y[[1:2]], fun = anglefun, ...)
    names(map)  <- c("angle", "magnitude")
    return(map)
}

