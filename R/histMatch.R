#' Image to Image Contrast Matching
#' 
#' Performs image to image contrast adjustments based on histogram matching using empirical cumulative distribution functions from both images.
#' 
#' @param x RasterLayer. Source raster which is to be modified.
#' @param ref RasterLayer. Reference raster, to which x will be matched.
#' @param nsamp integer. Number of random samples to build the histograms.
#' @param intersectOnly logical. If \code{TRUE} sampling will only take place in the overlap extent of the two rasters. Otherwise the full rasters will be used for sampling.
#' @param precise logical. If \code{TRUE} the exact same pixels will be used in the overlap.
#' @param ... Further arguments to be passed to \link[raster]{writeRaster}.
#' @return A RasterLayer of x adjusted to the histogram of ref.
#' @export
#' @examples 
#' 
#' par(mfrow=c(1,3))
#' r0  <- raster(system.file("external/test.grd", package="raster"))
#' plot(r0, main = "original meuse dataset")
#' 
#' r1 <- crop(r0, extent(c(180000,182000,330000,334000))) * 10
#' r.merge <- merge(r1, r0)  
#' plot(r.merge, main ="merge r0+r1 without histMatch")
#' 
#' r.match <- histMatch(r0, r1)
#' rcmatch <- merge(r1, r.match)
#' plot(rcmatch, main = "merge r0+r1 with histMatch")
histMatch <- function(x, ref, nsamp = 100000, intersectOnly = TRUE, precise = TRUE, ...){
    if(nsamp > ncell(ref)) nsamp <- ncell(ref)
    
    ## Define intersecting extent if required. Returns NULL if FALSE
    ext <- if(precise | intersectOnly) intersect(extent(x), extent(ref)) 
    if(precise & is.null(ext)) {
        precise <- FALSE
        warning("Rasters do not overlap. Precise sampling disabled.", call. = FALSE)
    }
    
    ## Sample histogram data
    ref.sample  <- sampleRandom(ref, nsamp, na.rm = T, ext = ext, xy = precise)
    if(precise) {
        x.sample   <- extract(x, ref.sample[,c("x","y")])
        ref.sample <- ref.sample[!is.na(x.sample),3] 
        xsamp      <- x.sample[is.na(x.sample)]
    } else {
        ref.sample  <- ref.sample[,3] 
        x.sample 	<- sampleRandom(x, nsamp, na.rm = T, ext = ext)
    }
    
    ## Estimate source empirical cumulative distribution function
    source.ecdf <- ecdf(x.sample)
    
    ## Estimate and invert reference ecdf
    ecdf.ref <- ecdf(ref.sample)
    kn 		 <- knots(ecdf.ref)
    y    	 <- ecdf.ref(kn) 
    limits   <- if(ref@data@haveminmax) c(ref@data@min, ref@data@max) else range(ref.sample)
    inverse.ref.ecdf <- approxfun(y, kn, method = "linear", yleft = limits[1] , yright = limits[2], ties = "ordered")
    
    ## Function definition
    histMatchFun <- if(grepl("INT", dataType(ref)))
                function(values, na.rm = FALSE){round( inverse.ref.ecdf( source.ecdf(values)))}
            else {
                function(values, na.rm = FALSE){       inverse.ref.ecdf( source.ecdf(values))}
            }
    
    ## Apply histMatch to raster 
    calc(x, histMatchFun, ...)
}
