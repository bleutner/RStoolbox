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
#' @param forceInteger logical. Force integer output.
#' @return A RasterLayer of x adjusted to the histogram of ref.
#' @references Richards and Jia: Remote Sensing Digital Image Analysis. Springer, Berlin, Heidelberg, Germany, 439pp.
#' @export
#' @examples 
#' library(ggplot2)
#' 
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
histMatch <- function(x, ref, nsamp = 1e5, intersectOnly = TRUE, precise = TRUE, forceInteger = FALSE, ...){
    if(nsamp > ncell(ref)) nsamp <- ncell(ref)
    
    ## Define intersecting extent if required. Returns NULL if FALSE
    ext <- if(precise | intersectOnly) intersect(extent(x), extent(ref)) 
    if(precise & is.null(ext)) {
        precise <- FALSE
        warning("Rasters do not overlap. Precise sampling disabled.", call. = FALSE)
    }
    
    ## Sample histogram data
    ref.sample  <- sampleRandom(ref, nsamp, na.rm = TRUE, ext = ext, xy = precise)
    if(precise) {
        x.sample   <- extract(x, ref.sample[,c("x","y")])
        if(is.vector(x.sample)) x.sample <- as.matrix(x.sample)
        valid <- complete.cases(x.sample)
        ref.sample <- ref.sample[valid, -c(1:2), drop = FALSE] 
        x.sample      <- x.sample[valid, , drop = FALSE]
    } else {
        ref.sample  <- ref.sample[,-c(1:2)] 
        x.sample 	<- sampleRandom(x, nsamp, na.rm = T, ext = ext)
    }
    
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
                histMatchFun <- if(grepl("INT", dataType(ref)) | forceInteger)
                            function(values, na.rm = FALSE){round( inverse.ref.ecdf( source.ecdf(values)))}
                        else {
                            function(values, na.rm = FALSE){       inverse.ref.ecdf( source.ecdf(values))}
                        }
                histMatchFun
            })
    ## Apply histMatch to raster 
    out <- stack(lapply(1:nlayers(x), FUN = function(i) raster::calc(x[[i]], fun = layerFun[[i]], ...)))
    names(out) <- names(x)
    out
}
