#' Linear image rescaling 
#' 
#' performs linear shifts of value ranges either to match another image
#' or to any other min and max value.
#' 
#' @param x Raster* object. Image to normalise.
#' @param y Raster* object. Reference image.
#' @param xmin Numeric. Min value of x.
#' @param xmax Numeric. Max value of x.
#' @param ymin Numeric. Min value of y.
#' @param ymax Numeric. Max value of y.
#' @param forceMinMax Logical. Forces update of min and max data slots in x or y.
#' 
#' @seealso \link{histMatch}
#' @export
rescaleImage <- function(x, y, xmin, xmax, ymin, ymax, forceMinMax = FALSE) {
    if(inherits(x, "Raster")){
        if(forceMinMax)  x <- setMinMax(x)
        if(!missing(y) && forceMinMax)  y <- setMinMax(y)
        if(missing("ymin")) ymin <- minValue(y)
        if(missing("ymax")) ymax <- maxValue(y)
        if(missing("xmin"))	xmin <- minValue(x) 
        if(missing("xmax")) xmax <- maxValue(x)
    } else {
        if(missing("xmin"))	xmin <- min(x, na.rm = T)
        if(missing("xmax")) xmax <- max(x, na.rm = T)
    }
    scal <- (ymax - ymin)/(xmax-xmin) 
    if(inherits(x, "Raster")){   
        x <- .paraRasterFun(x, rasterFun = calc,  args = list(fun = function(x) {(x - xmin) * scal + ymin}))
    } else {
        x <- (x - xmin) * scal + ymin
    }
    
    return(x)
}