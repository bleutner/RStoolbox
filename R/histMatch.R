#' Image to Image Contrast Matching
#' 
#' Performs image to image contrast adjustments based on histogram matching using empirical cumulative distribution functions from both images.
#' 
#' @param x RasterLayer. Source raster which is to be modified.
#' @param ref RasterLayer. Reference raster, to which x will be matched.
#' @param xmask RasterLayer. Mask layer for \code{x} to exclude pixels which might distort the histogram, i.e. are not present in \code{ref}. Any NA pixel in \code{xmask} will be ignored (\code{maskvalue = NA}). 
#' @param refmask RasterLayer. Mask layer for \code{ref}. Any NA pixel in \code{refmask} will be ignored (\code{maskvalue = NA}). 
#' @param nSamples integer. Number of random samples to build the histograms.
#' @param intersectOnly logical. If \code{TRUE} sampling will only take place in the overlap extent of the two rasters. Otherwise the full rasters will be used for sampling.
#' @param paired logical. If \code{TRUE} the corresponding pixels will be used in the overlap.
#' @param ... Further arguments to be passed to \link[raster]{writeRaster}.
#' @param forceInteger logical. Force integer output.
#' @return A RasterLayer of x adjusted to the histogram of ref.
#' @references Richards and Jia: Remote Sensing Digital Image Analysis. Springer, Berlin, Heidelberg, Germany, 439pp.
#' @export
#' @examples 
#' library(ggplot2)
#' library(raster)
#' data(rlogo)
#' ## Original image a (+1 to prevent log(0))
#' img_a <-  rlogo + 1 
#' ## Degraded image b
#' img_b <- log(img_a)
#' ## Cut-off half the image (just for better display)
#' img_b[, 1:50] <- NA
#' 
#' ## Compare Images before histMatching
#' ggRGB(img_a,1,2,3)+
#'         ggRGB(img_b, 1,2,3, ggLayer = TRUE, stretch = "lin", q = 0:1) +
#'         geom_vline(aes(xintercept = 50))+
#'         ggtitle("Img_a vs. Img_b")
#' 
#' ## Do histogram matching
#' img_b_matched <- histMatch(img_b, img_a)
#' 
#' ## Compare Images after histMatching
#' ggRGB(img_a, 1, 2, 3)+
#'         ggRGB(img_b_matched, 1, 2, 3, ggLayer = TRUE, stretch = "lin", q = 0:1) +
#'         geom_vline(aes(xintercept = 50))+
#'         ggtitle("Img_a vs. Img_b_matched")
#' 
#' ## Histogram comparison
#' opar <- par(mfrow = c(1, 3), no.readonly = TRUE)
#' img_a[,1:50] <- NA
#' redLayers <- stack(img_a, img_b, img_b_matched)[[c(1,4,7)]]
#' names(redLayers) <- c("img_a", "img_b", "img_b_matched")
#' 
#' hist(redLayers) 
#' ## Reset par 
#' par(opar)
histMatch <- function(x, ref, xmask = NULL, refmask = NULL, nSamples = 1e5, intersectOnly = TRUE, paired = TRUE, forceInteger = FALSE, ...){
    if(nSamples > ncell(ref)) nSamples <- ncell(ref)
    
    ## Define intersecting extent if required. Returns NULL if FALSE
    ext <- if(paired | intersectOnly) intersect(extent(x), extent(ref)) 
    if(paired & is.null(ext)) {
        paired <- FALSE
        warning("Rasters do not overlap. Precise sampling disabled.", call. = FALSE)
    }
    
    if(!is.null(xmask)) {
        .vMessage("Apply xmask")
        xfull <- x
        x <- mask(x, xmask)
     }
    if(!is.null(refmask)){
     .vMessage("Apply refmask")
        ref <- mask(ref, refmask)
    }
    ## Sample histogram data  
    .vMessage("Extract samples")

    ref.sample  <- sampleRandom(ref, size = nSamples, na.rm = TRUE, ext = ext, xy = paired)
 
    if(paired) {
        x.sample   <- extract(x, ref.sample[,c("x","y")])
        if(is.vector(x.sample)) x.sample <- as.matrix(x.sample)
        valid <- complete.cases(x.sample)
        ref.sample <- ref.sample[valid, -c(1:2), drop = FALSE] 
        x.sample      <- x.sample[valid, , drop = FALSE]
    } else {
        x.sample 	<- sampleRandom(x, size = nSamples, na.rm = T, ext = ext)
    }
    
    .vMessage("Calculate empirical cumulative histograms")
    layerFun <- lapply(1:ncol(x.sample), function(i) {
                ## Estimate source empirical cumulative distribution function
                source.ecdf <- ecdf(x.sample[,i])
                
                ## Estimate and invert reference ecdf
                ecdf.ref <- ecdf(ref.sample[,i])
                kn 		 <- knots(ecdf.ref)
                y    	 <- ecdf.ref(kn) 
                limits   <- if(.hasMinMax(ref[[i]])) c(minValue(ref)[i], maxValue(ref)[i]) else range(ref.sample)
                inverse.ref.ecdf <- approxfun(y, kn, method = "linear", yleft = limits[1] , yright = limits[2], ties = "ordered")
                
                ## Function definition
                histMatchFun <- if(grepl("INT", dataType(ref)[i]) | forceInteger)
                            function(values, na.rm = FALSE){round( inverse.ref.ecdf( source.ecdf(values)))}
                        else {
                            function(values, na.rm = FALSE){       inverse.ref.ecdf( source.ecdf(values))}
                        }
                histMatchFun    
            })
    
    totalFun <- function(x, f = layerFun) {
        app <- lapply(1:ncol(x), function(i) {
            f[[i]](x[,i])       
        })
        do.call("cbind", app)
    }
    ## Apply histMatch to raster 
    .vMessage("Apply histogram match functions")
    out <- raster::calc(x, fun = totalFun, forcefun = TRUE, ...)
    
    if(!is.null(xmask)) out <- merge(xfull, out)
    names(out) <- names(x)
    
    out
}
