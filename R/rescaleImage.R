#' Linear image rescaling 
#' 
#' performs linear shifts of value ranges either to match min and max of another image
#' or to any other min and max value.
#' 
#' @param x Raster* object. Image to normalise.
#' @param y Raster* object. Reference image.
#' @param xmin Numeric. Min value of x.
#' @param xmax Numeric. Max value of x.
#' @param ymin Numeric. Min value of y.
#' @param ymax Numeric. Max value of y.
#' @param forceMinMax Logical. Forces update of min and max data slots in x or y.
#' @return 
#' Returns a Raster* object of the same dimensions as the input raster \code{x} but shifted and stretched to the new limits.
#' @seealso \link{histMatch}
#' @export
#' @examples 
#' ## Create example data
#' data(lsat)
#' lsat2 <- lsat - 1000
#' lsat2
#' 
#' ## Rescale lsat2 to match original lsat value rang
#' lsat2_rescaled <- rescaleImage(lsat2, lsat)
#' lsat2_rescaled
#' 
#' ## Rescale lsat to value range [0,1]
#' lsat2_unity <- rescaleImage(lsat2, ymin = 0, ymax = 1)
#' lsat2_unity
rescaleImage <- function(x, y, xmin, xmax, ymin, ymax, forceMinMax = FALSE) {
    
    if(inherits(x, "Raster")){
        if(!missing("y") && nlayers(x) != nlayers(y)) stop("x and y must have the same number of layers")
        if(forceMinMax)  x <- setMinMax(x)
        if(!missing("y") && forceMinMax)  y <- setMinMax(y)
        if(missing("ymin")) ymin <- minValue(y)
        if(missing("ymax")) ymax <- maxValue(y)
        if(missing("xmin"))	xmin <- minValue(x) 
        if(missing("xmax")) xmax <- maxValue(x)
    } else {
        if(missing("xmin"))	xmin <- min(x, na.rm = T)
        if(missing("xmax")) xmax <- max(x, na.rm = T)
    }
    
    norange <- xmax == xmin    
    norange[is.na(norange)] <- TRUE 
    if(any(norange)) {
        warning("x has no value range (xmin = xmax) or NAs only. Rescaling is not possible.\n  Will return unchanged values. Affected bands: ", paste0(names(x)[norange], collapse = ", "))       
    } else {
        scal <- (ymax - ymin)/(xmax-xmin) 
        if(inherits(x, "Raster")){   
            x <- .paraRasterFun(x, rasterFun = calc,  args = list(fun = function(x) {(x - xmin) * scal + ymin}, forcefun = TRUE))
        } else {
            x <- (x - xmin) * scal + ymin
        }
    }
        
    return(x)
}





