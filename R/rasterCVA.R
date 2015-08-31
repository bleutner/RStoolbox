#' Change Vector Analysis
#' 
#' Calculateds angle and magnitude of change vectors. 
#' Dimensionality is limited to two bands. 
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
#' mtlFile  <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")
#' lsat  <- stackMeta(mtlFile)
#' img_a <- lsat[[3:4]]
#' mult  <- raster(lsat)
#' mult[]<-rnorm(ncell(mult))
#' img_b <- img_a * mult + 10
#' 
#' ## Do change vector analysis
#' cva <- rasterCVA(img_a, img_b)
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

