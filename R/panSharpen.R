#' Pan Sharpen Imagery / Image Fusion
#' 
#' provides different methods for pan sharpening a coarse resolution (typically multispectral) image with 
#' a higher reolution panchromatic image. Values of the pan-chromatic and multispectral images must be of the same scale, (e.g. from 0:1, or all DNs from 0:255)
#' 
#' @param img Raster* object. Coarse resolution multispectral image 
#' @param pan RasterLayer. High resolution image, typically panchromatic.
#' @param method Character. Choose method from c("pca", "ihs", "brovey").
#' @param r Character or Integer. Red band in \code{img}. Only relevant if \code{method!='pca'}
#' @param g Character or Integer. Green band in \code{img}. Only relevant if \code{method!='pca'}
#' @param b Character or Integer. Blue band in \code{img}. Only relevant if \code{method!='pca'}
#' @param pc Integer. Only relevant if \code{method = 'pca'}. Which principal component to replace. Usually this should be the first component (default). Only if the first component is dominated by something else than brightness it might be worth a try to use the second component.
#' @param norm Logical.  Rescale pan image to match the 1st PC component. Only relevant if \code{method = 'pca'}. If \code{TRUE} only min and max are matched to the 1st PC. If \code{FALSE} pan will be histogram matched to the 1st PC. 
#' @details 
#' Pan sharpening options:
#' \itemize{ 
#'  \item{\code{method='pca'}: Performs a pca using \link{rasterPCA}. The first component is then swapped for the pan band an the PCA is rotated backwards.}
#'  \item{\code{method='ihs'}: Performs a color space transform to Intensity-Hue-Saturation space, swaps intensity for the histogram matched pan and does the backwards transformation.}
#' 	\item{\code{method='brovey'}: Performs Brovey reweighting. Pan and img must be at the same value scale (e.g. 0:1, or 0:255) otherwise you'll end up with psychodelic colors.}
#' }
#' @export
#' @examples 
#' library(raster)
#' library(ggplot2)
#' 
#' ## Load example data
#' data(lsat)
#' ## Fake panchromatic image (30m resolution, wavelength: visible (integral from blue to red)
#' pan       <- sum(lsat[[1:3]]) 
#' ggR(pan, stretch = "lin") 
#' 
#' ## Fake coarse resolution image (150m spatial resolution)
#' lowResImg <- aggregate(lsat, 5) 
#' 
#' 
#' ## Brovey pan sharpening
#' lowResImg_pan <- panSharpen(lowResImg, pan, r = 3, g = 2, b = 1, method = "brovey")
#' lowResImg_pan
#' ## Plot 
#' ggRGB(lowResImg, stretch = "lin") + ggtitle("Original")
#' ggRGB(lowResImg_pan, stretch="lin") + ggtitle("Pansharpened (Brovey)")
#' 	
panSharpen <- function(img, pan, r, g, b, pc = 1, method = "brovey", norm = TRUE) {
    ## TODO: add weighting
	stopifnot(inherits(img, "Raster") & inherits(pan, "Raster"))
	if(res(img)[1] <= res(pan)[1]) stop("Pan image must be of higher spatial resolution than img.")
		
    if(method == "pca") {
        layernames <- names(img) 
    } else {
        layernames <- names(img)[c(r,g,b)] 
        ordr <- match(layernames, names(img))
        layernames <- layernames[order(ordr)]
    }
    
    if(method == "pca") {
        imgpca <- rasterPCA(img)
        
        imgpcaHiRes <- raster::resample(imgpca$map, pan, method = "ngb")
        
        if(norm) {
            panMa <- rescaleImage(pan, imgpca$map[[1]])
        }else{
            panMa <- histMatch(pan, imgpca$map[[1]])
        }
        imgpcaHiRes[[pc]] <- panMa
        eigen  <- t(loadings(imgpca$model))
        cents  <- imgpca$model$center
        panimg <- .paraRasterFun(imgpcaHiRes, rasterFun = calc, args = list(fun = function(x) { x %*%  eigen  +  cents}))
    }
    if(method == "ihs")     {  
        #xmax <- .DATATYPEdb[dataType(img[[c(r,g,b)]]),"max"]
        #img <- rescaleImage(img[[c(r,g,b)]], xmin = 0, xmax = xmax, ymin=0, ymax=1)          
        
        Mfwd <- t(matrix(c(rep(1/3,3), rep(sqrt(6)^-1,2), -2/sqrt(6), 1/sqrt(2), -1/sqrt(2), 0), ncol=3, byrow = T))   ## Carper1990
        Mbwd <- t(Mfwd)
        Mbwd[1,] <- 1  
        
        Ivv    <- .paraRasterFun(img[[c(r,g,b)]], rasterFun = calc, args = list(fun = function(x) x %*% Mfwd))
        Ivvr   <- raster::resample(Ivv[[2:3]], pan, method = "bilinear")
        panMa  <- histMatch(x = pan, ref = Ivv[[1]], paired = FALSE, intersectOnly = FALSE)
        panimg <- .paraRasterFun(stack(panMa, Ivvr) , rasterFun = calc, args = list(fun = function(x) x %*% Mbwd))
    }
    if(method == "brovey"){
        msi    <- resample(img[[layernames]], pan, method = "ngb")
        mult   <- pan / sum(msi)
        panimg <- msi * mult
    }
    names(panimg) <- paste0(layernames, "_pan")
    panimg
}


