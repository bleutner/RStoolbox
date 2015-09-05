#' Pseudo-Invariant Features based Image Matching
#' 
#' Match one scene to another based on linear regression of pseudo-invariant features (PIF).
#' 
#' @param img RasterStack or RasterBrick. Image to be adjusted.
#' @param ref RasterStack or RasterBruck. Reference image.
#' @param method Method to calculate pixel similariry. Options: euclidean distance ('ed'), spectral angle
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
#' Returns a RasterStack with the adjusted image (\code{img}) by default. 
#' 
#' If further return arguments are specified the following intermediate products will be returned in a list along with the adujsted image:
#' \itemize{
#'    \item \code{returnModels = TRUE}: returns the linear models; one for each layer. 
#'    \item \code{returnPifMap = TRUE}: returns binary map of pixels which were selected as pseudo-invariant features. 
#'    \item \code{returnSimMap = TRUE}: returns the pixel-wise similarity.
#' }
#' @export 
#' @examples 
#' library(gridExtra)
#' ## Import Landsat example data
#' data(lsat)
#' 
#' ## Create fake example data
#' ## In practice this would be an image from another acquisition date
#' lsat_b <- log(lsat)
#' ## Run pifMatch
#' lsat_b_adjusted <- pifMatch(lsat_b, lsat)
#' 
#' ## Run pifMatch and return similarity layer and pifMap
#' lsat_b_adjusted <- pifMatch(lsat_b, lsat, returnPifMap = TRUE, returnSimMap = TRUE)
#' \donttest{grid.arrange(
#' ggR(lsat_b_adjusted$simMap, geom_raster = TRUE) ,
#' ggR(lsat_b_adjusted$pifMap),
#' ncol=2)}
pifMatch <- function(img, ref, method = "cor", quantile = 0.95, returnPifMap = FALSE, returnSimMap = TRUE, returnModels = FALSE){
	stopifnot(nlayers(img)==nlayers(ref) && nlayers(img) > 1)
	
	imgfull <- img
	## Get joint extent
	if(!extent(img)==extent(ref)) { 
		img <- crop(img, ref)
		ref <- crop(ref, img)
	}
	
	## Calculate pixelwise similarity
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
	
	out <- list(models = models, simMap = pifield, pifMap = pi, img = correct)
	ret <- c(
			if(returnSimMap) "simMap",
			if(returnPifMap) "pifMap",
			if(returnModels) "models"
	)
	out[!names(out) %in% ret] <- NULL
	return(out)
} 






