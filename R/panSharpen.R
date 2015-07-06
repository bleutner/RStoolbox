#' Pan sharpen imagery / Image fusion
#' 
#' provides different methods for pan sharpening a coarse resolution (typically multispectral) image with 
#' a higher reolution panchromatic image. Values of the pan-chromatic and multispectral images must be of the same scale, (e.g. from 0:1, or all DNs from 0:255)
#' 
#' @param img Raster* object. Coarse resolution imagery 
#' @param pan RasterLayer. High resolution image, typically panchromatic.
#' @param method Character. Choose method from c("pca", "ihs", "brovey").
#' @param r Character or Integer. Red band in \code{img}. Only relevant if \code{method!='pca'}
#' @param g Character or Integer. Green band in \code{img}. Only relevant if \code{method!='pca'}
#' @param b Character or Integer. Blue band in \code{img}. Only relevant if \code{method!='pca'}
#' @param pc Integer. Which principal component to replace. Usually this should be the first component (default). Only if the first component is dominated by something else than brightness it might be worth a try to use the second component.
#' @param norm Logical. Normalize pan image to 1st PC component. If \code{FALSE} pan will be histogram matched to the 1st PC. Otherwise only min and max are matched.
#' @details 
#' Pan sharpening options:
#' \itemize{ 
#'  \item{\code{method='pca'}: Performs a pca using \link{rasterPCA}. The first component is then swapped for the pan band an the PCA is rotated backwards.}
#'  \item{\code{method='ihs'}: Performs a color space transform to Intensity-Hue-Saturation space, swaps intensity for the histogram matched pan and does the backwards transformation.}
#' 	\item{\code{method='brovey'}: Performs Brovey reweighting. Pan and img must be at the same value scale (e.g. 0:1, or 0:255) otherwise you'll end up with psychodelic colors.}
#' }
#' @export
panSharpen <- function(img, pan, r, g, b, pc = 1, method = "brovey", norm=TRUE) {
    ## TODO: add weighting
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
        eigen <- t(loadings(imgpca$model))
        cents <- imgpca$model$center
        panimg <- .paraRasterFun(imgpcaHiRes, rasterFun = calc, args = list(fun = function(x) { x %*%  eigen  +  cents}))
    }
    if(method == "ihs")     {  
        #xmax <- .DATATYPEdb[dataType(img[[c(r,g,b)]]),"max"]
        #img <- rescaleImage(img[[c(r,g,b)]], xmin = 0, xmax = xmax, ymin=0, ymax=1)          
        
        Mfwd <- t(matrix(c(rep(1/3,3), rep(sqrt(6)^-1,2), -2/sqrt(6), 1/sqrt(2), -1/sqrt(2), 0), ncol=3, byrow = T))   ## Carper1990
        Mbwd <- t(Mfwd)
        Mbwd[1,] <- 1  
        
        Ivv   <- .paraRasterFun(img[[c(r,g,b)]], rasterFun = calc, args = list(fun = function(x) x %*% Mfwd))
        Ivvr  <- raster::resample(Ivv[[2:3]], pan, method = "bilinear")
        panMa <- histMatch(pan, Ivv[[1]])
        panimg   <- .paraRasterFun(stack(panMa, Ivvr) , rasterFun = calc, args = list(fun = function(x) x %*% Mbwd))
    }
    if(method == "brovey"){
        msi <- resample(img[[layernames]], pan, method = "ngb")
        mult <- pan / sum(msi)
        panimg <- msi * mult
    }
    names(panimg) <- paste0(layernames, "_pan")
    panimg
}


