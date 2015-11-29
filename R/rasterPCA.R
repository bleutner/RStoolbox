#' Principal Component Analysis for Rasters
#' 
#' Calculates R-mode PCA for RasterBricks or RasterStacks and returns a RasterBrick with multiple layers of PCA scores. 
#' 
#' Internally rasterPCA relies on the use of \link[stats]{princomp} (R-mode PCA). If nSamples is given the PCA will be calculated
#' based on a random sample of pixels and then predicted for the full raster. If nSamples is NULL then the covariance matrix will be calculated
#' first and will then be used to calculate princomp and predict the full raster. The latter is more precise, since it considers all pixels,
#' however, it may be slower than calculating the PCA only on a subset of pixels. 
#' 
#' Pixels with missing values in one or more bands will be set to NA. The built in check for such pixels can lead to a slow-down of rasterPCA.
#' However, if you make sure or know beforehand that all pixels have either only valid values or only NAs throughout all layers you can disable this check
#' by setting maskCheck=FALSE which speeds up the computation.
#' 
#' Standardised PCA (SPCA) can be usefull if imagery or bands of different dynamic ranges are combined. SPC uses the correlation matrix instead of the covariance matrix, which
#' has the same effect as using normalised bands of unit variance. 
#' 
#' @param img RasterBrick or RasterStack.
#' @param nSamples Integer or NULL. Number of pixels to sample for PCA fitting. If NULL, all pixels will be used.
#' @param nComp Integer. Number of PCA components to return.
#' @param norm Logical. Center and normalize image befor calculating PCA. It is usually benefitial to do this. 
#' @param spca Logical. If \code{TRUE}, perform standardized PCA.
#' @param maskCheck Logical. Masks all pixels which have at least one NA (default TRUE is reccomended but introduces a slowdown, see Details when it is wise to disable maskCheck). 
#' Takes effect only if nSamples is NULL.
#' @param ... further arguments to be passed to \link[raster]{writeRaster}, e.g. filename.
#' @return RasterBrick
#' @details
#' The norm argument should in most cases be set to TRUE  (unless your input img is already normalized). It subtracts the mean and divides by the standard deviation.
#' @export 
#' @examples 
#' library(ggplot2)
#' library(reshape2)
#' data(rlogo)
#' ggRGB(rlogo, 1,2,3)
#' 
#' ## Run PCA
#' set.seed(25)
#' rpc <- rasterPCA(rlogo)
#' rpc
#' summary(rpc$model)
#' 
#' ggRGB(rpc$map,1,2,3, stretch="lin", q=0)
#' if(require(gridExtra)){
#' plots <- lapply(1:3, function(x) ggR(rpc$map, x, geom_raster = TRUE))
#' grid.arrange(plots[[1]],plots[[2]], plots[[3]], ncol=2)
#' }

rasterPCA <- function(img, nSamples = NULL, nComp = nlayers(img), spca = FALSE, norm = TRUE, maskCheck = TRUE, ...){      
    
    if(nlayers(img) <= 1) stop("Need at least two layers to calculate PCA.")    
    if(nComp > nlayers(img)) nComp <- nlayers(img)
    
    if(norm) img <- normImage(img)
     
    if(!is.null(nSamples)){    
        trainData <- sampleRandom(img, size = nSamples, na.rm = TRUE)
        if(nrow(trainData) < nlayers(img)) stop("nSamples too small or img contains a layer with NAs only")
        model <- princomp(trainData, scores = FALSE, cor = spca)
    } else {
        if(maskCheck) {
            totalMask <- !sum(calc(img, is.na))
            if(cellStats(totalMask, sum) == 0) stop("img contains either a layer with NAs only or no single pixel with valid values across all layers")
            img <- mask(img, totalMask , maskvalue = 0) ## NA areas must be masked from all layers, otherwise the covariance matrix is not non-negative definite   
        }
        st <- if(spca) "pearson" else "cov"
        covMat <- layerStats(img, stat = st, na.rm = TRUE)
        model  <- princomp(covmat = covMat[[1]], cor=spca)
        model$center <- covMat$mean
    }
    ## Predict
    out   <- .paraRasterFun(img, rasterFun=raster::predict, args = list(model = model, na.rm = TRUE, index = 1:nComp), wrArgs = list(...))  
    names(out) <- paste0("PC", 1:nComp)
    structure(list(call = match.call(), model = model, map = out), class = c("rasterPCA", "RStoolbox"))  
    
}



