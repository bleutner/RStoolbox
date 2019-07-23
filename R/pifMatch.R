#' Pseudo-Invariant Features based Image Matching
#' 
#' Match one scene to another based on linear regression of pseudo-invariant features (PIF).
#' 
#' @param img RasterStack or RasterBrick. Image to be adjusted.
#' @param ref RasterStack or RasterBruck. Reference image.
#' @param method Method to calculate pixel similariry. Options: euclidean distance ('ed'), spectral angle ('sam') or pearson correlation coefficient ('cor').
#' @param quantile Numeric. Threshold quantile used to identify PIFs
#' @param returnPifMap Logical. Return a binary raster map ot pixels which were identified as pesudo-invariant features.
#' @param returnSimMap Logical. Return the similarity map as well
#' @param returnModels Logical. Return the linear models along with the adjusted image.
#' @details 
#' The function consists of three main steps:
#' First, it calculates pixel-wise similarity between the two rasters and identifies pseudo-invariant pixels based on 
#' a similarity threshold. 
#' In the second step the values of the pseudo-invariant pixels are regressed against each other in a linear model for each layer.
#' Finally the linear models are applied to all pixels in the \code{img}, thereby matching it to the reference scene.
#' 
#' Pixel-wise similarity can be calculated using one of three methods: euclidean distance (\code{method = "ed"}), spectral angle (\code{"sam"}) or pearsons correlation coefficient (\code{"cor"}).
#' The threshold is defined as a similarity quantile. Setting \code{quantile=0.95} will select all pixels with a similarity above the 95\% quantile as pseudo-invariant features.
#' 
#' Model fitting is performed with simple linear models (\code{\link[stats]{lm}}); fitting one model per layer. 
#' @return 
#' Returns a List with the adjusted image and intermediate products (if requested). 
#' #' \itemize{
#'    \item \code{img}: the adjusted image
#'    \item \code{simMap}: pixel-wise similarity map (if \code{returnSimMap = TRUE})
#'    \item \code{pifMap}: binary map of pixels selected as pseudo-invariant features (if \code{returnPifMap = TRUE}) 
#'    \item \code{models}: list of linear models; one per layer (if \code{returnModels = TRUE})                          
#' }
#' @export 
#' @examples 
#' library(raster)
#' 
#' ## Import Landsat example data
#' data(lsat)
#' 
#' ## Create fake example data
#' ## In practice this would be an image from another acquisition date
#' lsat_b <- log(lsat)  
#' 
#' ## Run pifMatch and return similarity layer, invariant features mask and models
#' lsat_b_adj <- pifMatch(lsat_b, lsat, returnPifMap = TRUE, 
#'                          returnSimMap = TRUE, returnModels = TRUE)
#' \donttest{
#' ## Pixelwise similarity
#' ggR(lsat_b_adj$simMap, geom_raster = TRUE)
#' 
#' ## Pesudo invariant feature mask 
#' ggR(lsat_b_adj$pifMap)
#' 
#' ## Histograms of changes
#' par(mfrow=c(1,3))
#' hist(lsat_b[[1]], main = "lsat_b")
#' hist(lsat[[1]], main = "reference")
#' hist(lsat_b_adj$img[[1]], main = "lsat_b adjusted")
#' 
#' ## Model summary for first band
#' summary(lsat_b_adj$models[[1]])
#' }
pifMatch <- function(img, ref, method = "cor", quantile = 0.95, returnPifMap = TRUE, returnSimMap = TRUE, returnModels = FALSE){
    if(nlayers(img)!=nlayers(ref) | nlayers(img) <= 1) stop("Both images need at least two corresponding bands and must have the same number of bands.", call.=FALSE)
    
    imgfull <- img
    ## Get joint extent
    if(!extent(img)==extent(ref)) { 
        img <- crop(img, ref)
        ref <- crop(ref, img)
    }
    
    ## Calculate pixelwise similarity
    if(!method %in% c("ed", "sam", "cor")) stop("method must be one of 'ed', 'cor' or 'sam'", call. = FALSE)
    nmeth <- c(ed=1, sam=2, cor=3)[method]    
    pifield <- overlay(img, ref, fun = function(x,y) {pwSimilarityCpp(x,y,nmeth)})
    names(pifield) <- method
    ## Similarity quantile threshold
    thresh  <- quantile(pifield, p = quantile)
    pi      <- pifield > thresh
    names(pi) <- "pifMap"
    
    ## Fit linear models
    models <- lapply(1:nlayers(img), function(i){
                df <- data.frame(img = img[[i]][pi], ref = ref[[i]][pi])
                mod <- lm(ref~img, data = df)
            })
    
    ## Predict linear models for whole raster
    correct <- lapply(seq_along(models), function(i){
                corLayer <- imgfull[[i]]
                mod <- models[[i]]
                names(corLayer) <- "img"
                predict(corLayer, mod)
            })
    
    ## Assemble and return output
    names(models) <- names(img)
    correct <- stack(correct)
    names(correct) <- names(imgfull)
    
    out <- list( img = correct, simMap = pifield, pifMap = pi, models = models )
    ret <- c("img",
            if(returnSimMap) "simMap",
            if(returnPifMap) "pifMap",
            if(returnModels) "models"
    )
    out[!names(out) %in% ret] <- NULL
    return(out)
} 






