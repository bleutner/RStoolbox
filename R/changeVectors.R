#' Change Vector Analysis
#' 
#' Calculateds angle and magnitude of change vectors. 
#' Dimensionality is limited to two bands.
#' 
#' @param x RasterBrick or RasterStack with two layers.
#' @param y RasterBrick or RasterStack with two layers.
#' @param ... further arguments passed to writeRaster
#' @export 
#' 
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

