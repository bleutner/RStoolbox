#' Linear Image Rescaling 
#' 
#' performs linear shifts of value ranges either to match min/max of another image (`y`)
#' or to any other min and max value (`ymin` and `ymax`).
#' 
#' Providing `xmin` and `xmax` values manually can be useful if the raster contains a variable of a known, fixed value range,
#' e.g. NDVI from -1 to 1 but the actual pixel values don't encompass this entire range. 
#' By providing `xmin = -1` and `xmax = 1` the values can be rescaled to any other range,
#' e.g. 1 to 100 while comparability to other rescaled NDVI scenes is retained. 
#' 
#' @param x Raster* object or SpatRaster or numeric vector. Image to normalise.
#' @param y Raster* object or SpatRaster or numeric vector. Reference image. Optional. Used to extract min and max values if ymin or ymax are missing.
#' @param xmin Numeric. Min value of x. Either a single value or one value per layer in x. If xmin is not provided it will be extracted from x.
#' @param xmax Numeric. Max value of x. Either a single value or one value per layer in x. If xmax is not provided it will be extracted from x.
#' @param ymin Numeric. Min value of y. Either a single value or one value per layer in x. If ymin is not provided it will be extracted from y.
#' @param ymax Numeric. Max value of y. Either a single value or one value per layer in x. If ymax is not provided it will be extracted from y.
#' @param forceMinMax Logical. Forces update of min and max data slots in x or y.
#' @param ... additional arguments passed to [terra::writeRaster()] 
#' @return 
#' Returns a SpatRaster of the same dimensions as the input raster \code{x} but shifted and stretched to the new limits.
#' @seealso [histMatch][histMatch()]
#' @export
#' @md
#' @examples
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
rescaleImage <- function(x, y, xmin, xmax, ymin, ymax, forceMinMax = FALSE, ...) {
	x <- .toTerra(x)
	if(!missing("y")) y <- .toTerra(y)
	
  if(inherits(x, "SpatRaster")){
    rn <- names(x)
    if(!missing("y") && nlyr(x) != nlyr(y)) stop("x and y must have the same number of layers")
    x <- terra::setMinMax(x, force = forceMinMax)
    if(!missing("y") && (missing("ymin") || missing("ymax"))) {
      y <- terra::setMinMax(y, force = forceMinMax)
      mm <- minmax(y)
      ymin <- mm[1,,drop = TRUE]
      ymax <- mm[2,,drop = TRUE]
    }
    if(missing("xmin") || missing("xmax")) {
      mm <- minmax(x)
      xmin <- mm[1,,drop = TRUE]
      xmax <- mm[2,,drop = TRUE]
    }
  } else {
    ## X and Y are vectors 
    if(missing("xmin")) xmin <- min(x, na.rm = T)
    if(missing("xmax")) xmax <- max(x, na.rm = T)
    if(missing("xmin")) ymin <- min(y, na.rm = T)
    if(missing("xmax")) ymax <- max(y, na.rm = T)
  }
  
  li <- list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  if(inherits(x, "SpatRaster")) {
    li <- lapply(li, function(lim){
      if(length(lim) == 1) {
        return(rep(lim, nlyr(x)))
      } else if(length(lim) == nlyr(x)){
        return(lim)
      }
      stop("xmin, xmax, ymin and ymax must be of length 1 or of length nlyr(x)", call. = FALSE)
    })
  }
  
  exclude <- li$xmax == li$xmin    
  finite  <- colSums(do.call(rbind, lapply(li,is.finite)))
  exclude[finite < 4] <- TRUE 
  if(any(exclude))  {
       warning("x has no value range (xmin = xmax) or non-finite values only. Rescaling is not possible.\n  Returning NA for bands: ",
    paste0(names(x)[exclude], collapse = ", "))   
  }
  
  if(all(exclude)) {
    x[]<-NA
    if("filename" %in% names(list(...))) {
      x <- terra::writeRaster(x, ...)
    }
    return(x)
  }
  
  scal <- (li$ymax - li$ymin)/(li$xmax - li$xmin)
  scal[exclude] <- NA
  if(inherits(x, "SpatRaster")){   
    x <- app(x, fun = function(x) {
      rescaleImageCpp(x, scal = scal, xmin = li$xmin, ymin = li$ymin)},
      ...)
    names(x) <- rn
  } else {
    x <- (x - li$xmin) * scal + li$ymin
  }
  return(x)
}





