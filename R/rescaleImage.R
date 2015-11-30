#' Linear Image Rescaling 
#' 
#' performs linear shifts of value ranges either to match min and max of another image (\code{y})
#' or to any other min and max value (\code{ymin} and \code{ymax}).
#' 
#' Providing xmin and xmax values manually can be useful if the raster contains a variable of a known, fixed value range, e.g. NDVI from -1 to 1 but the actual pixel values don't 
#' encompass this entire range. By providing xmin = -1 and xmax = 1 the values can be rescaled to any other range, e.g. 1 to 100 while comparability to other rescaled NDVI scenes is retained. 
#' 
#' @param x Raster* object. Image to normalise.
#' @param y Raster* object. Reference image. Optional. Used to extract min and max values if ymin or ymax are missing. 
#' @param xmin Numeric. Min value of x. Either a single value or one value per layer in x. If xmin is not provided it will be extracted from x.
#' @param xmax Numeric. Max value of x. Either a single value or one value per layer in x. If xmax is not provided it will be extracted from x.
#' @param ymin Numeric. Min value of y. Either a single value or one value per layer in x. If ymin is not provided it will be extracted from y.
#' @param ymax Numeric. Max value of y. Either a single value or one value per layer in x. If ymax is not provided it will be extracted from y.
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
#' ## Rescale lsat2 to match original lsat value range
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
        
        
        li <- list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
        li <- lapply(li, function(lim){
                    if(length(lim) == 1) {
                        return(rep(lim, nlayers(x)))
                    } else if(length(lim) == nlayers(x)){
                        return(lim)
                    }
                    stop("xmin, xmax, ymin and ymax must be of length 1 or of length nlayers(x)", call. = FALSE)
                })
        list2env(li, environment())
    } else {
        if(missing("xmin"))	xmin <- min(x, na.rm = T)
        if(missing("xmax")) xmax <- max(x, na.rm = T)
    }
    
    norange <- xmax == xmin    
    norange[is.na(norange)] <- TRUE 
    if(any(norange))   warning("x has no value range (xmin = xmax) or non-finite values only. Rescaling is not possible.\n  Returning NA for bands: ", paste0(names(x)[norange], collapse = ", "))       
    
    scal <- (ymax - ymin)/(xmax-xmin) 
    if(inherits(x, "Raster")){   
        x <- .paraRasterFun(x, rasterFun = calc,  args = list(fun = function(x) {
                            if(!inherits(x, "Matrix")) x <- as.matrix(x)
                            rescaleImageCpp(x, scal = scal, xmin = xmin, ymin = ymin)}, forcefun = T))
    } else {
        x <- (x - xmin) * scal + ymin
    }
    
    return(x)
}





