#' Change Vector Analysis
#' 
#' Calculates angle and magnitude of change vectors. 
#' Dimensionality is limited to two bands per image. 
#' 
#' @param x RasterBrick or RasterStack with two layers. This will be the reference/origin for the change calculations. Both rasters (y and y) need to correspond to each other, i.e. same resolution, extent and origin.
#' @param y RasterBrick or RasterStack with two layers. Both rasters (y and y) need to correspond to each other, i.e. same resolution, extent and origin.
#' @param tmf Numeric. Threshold median factor. Used to calculate a threshold magnitude for which pixels are considered stable, i.e. no change. Defaults to 2 times the median non-zero magnitude. Calculated as \code{tmf * median(magnitude[magnitude > 0])}  
#' @param ... further arguments passed to writeRaster
#' @details 
#' Change Vector Analysis (CVA) is used to identify spectral changes between two identical scenes which were acquired at different times. 
#' CVA is limited to two bands per image. For each pixel it calculates the change vector in the two-dimensional spectral space. 
#' For example for a given pixel in image A and B for the red and nir band the change vector is caclulated for the coordinate pairs: (red_A | nir_A) and (red_B | nir_B).
#' 
#'  The coordinate system is defined by the order of the input bands: the first band defines the x-axis and the second band the y-axis, respectively.
#'  Angles are returned *in degree* beginning with 0 degrees pointing 'north', i.e. the y-axis, i.e. the second band.
#' 
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
rasterCVA <- function(x, y, tmf = 2, ...) {
    if(nlayers(x) != 2 | nlayers(y) != 2) stop("need two rasters with two layers each")
    
    anglefun <- function(x, y) {
        magout <- angle <- rep(0, nrow(x))
        dif <- y - x
        magnitude <- sqrt(rowSums(dif^2))
        threshold <- tmf * median(magnitude[magnitude > 0], na.rm = TRUE)
        sel <- magnitude > threshold & !is.na(magnitude)
        if(any(sel)){
            magout[sel] <- magnitude[sel]
            angle[sel]  <- atan2(dif[sel,1],dif[sel,2]) / pi * 180
        }
        negang <- angle < 0
        angle[negang] <- 360 + angle[negang] 
        
        angle[is.na(magnitude)]  <- NA
        magout[is.na(magnitude)] <- NA
        
        m <- cbind(angle, magout)
    }
    
    map <- overlay(x, y, fun = anglefun, ...)
    names(map)  <- c("angle", "magnitude")
    return(map)
}

