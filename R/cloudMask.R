#' Very simple cloud detection for imagery with blue and thermal bands
#' 
#' @param x RasterBrick or RasterStack with reflectance and brightness temperature OR the mask of a previous run of \code{cloudMask} with \code{returnDiffLayer=TRUE}. 
#' @param threshold cloud detection threshold. If not provided it will be guessed. Everything *above* this threshold will be considered a cloud pixel (unless it is removed by filtering afterwards).
#' @param minCloudSize minimum number of cloud pixels in window1 
#' @param windowSize1 odd number, rectangular moving window to remove clouds which arre too small (likely artefacts)
#' @param windowSize2 odd number, rectangular buffer around cluster centers
#' @param sanitize logical. Should small clouds (possibly false positives) be removed by filtering? If \code{TRUE} windowSize1 must be specified.
#' @param maskGrowing logical. Applies simple region-growing (rectangular buffering) to the cloud mask. If \code{TRUE} windowSize2 must be specified.
#' @param lowBand bandname or number for the blue band
#' @param tirBand bandname or number for the thermal band
#' @param plot logical. Provides plots of the cloud mask for all sub-steps (sanitizing etc.) Helpful to find proper parametrization.
#' @param verbose logical. Print messages or supress.
#' @param returnDiffLayer logical. If \code{TRUE}, the difference layer will be returned along with the cloudmask. This option allows to re-use the difference layer in cloudMask.
#' @note Typically clouds are cold in the thermal region and have high reflectance in short wavelengths (blue). By differencing the two bands and thresholding a rough cloud mask can be obtained.
#' More sophisticated approaches can be found elsewhere, e.g. \link[https://code.google.com/p/fmask/]{fmask}.
#' 
#' It can make sense to find a suitable threshold on a cropped version of the scene. Also make sure you make use of the \code{returnDiffLayer} argument to save yourself one processing step.
#' Sanitizing and region growing can be seen as final polishing, i.e. as long as the pure cloud centers are not detected properly, you can turn those two arguments off if they take too long to calculate.
#' Once your mask detects obvious cloud pixels properly re-enable sanitizing and regionGrowing for fine tuning if desired. Finally, once a suitable threshold is established re-run cloudMask on the whole scene with this threshold and go get a coffee.
#' @export
#' @examples 
#' \dontrun{
#' ls <- stackMeta("path/to/MTL.txt")
#' ls_cor <- radCor(ls, "path/to/MTL.txt") 
#' ls_cmask <-cloudMask(ls_cor, returnDiffLayer = TRUE)
#' }
cloudMask <- function(x, threshold, minCloudSize, windowSize1 = 5, windowSize2 = 11, maskGrowing = TRUE, sanitize = TRUE, lowBand = "B1", tirBand = "B6", plot = TRUE, verbose = TRUE, returnDiffLayer = FALSE){
	
	## Set-up graphics device
	op <- par(mfrow = c(2, 1 + sum(sanitize, maskGrowing)))
	
	## Calculate or re-reuse cloud difference layer	
	if("CDIFF" %in% names(x)) {
		if(verbose) message("Re-using CDIFF layer from previous run.")
		cdiff <- x[["CDIFF"]]
	} else {
		cdiff <- x[[lowBand]] - x[[tirBand]]
		names(cdiff) <- "CDIFF"
	}
	
	## Guess threshold
	if(missing(threshold)) {
		threshold <- quantile(cdiff@data@max:cdiff@data@min, 0.45)
		if(verbose) {message(paste0("Estimated cloud threshold should be between ", round(cdiff@data@min), " and ", round(cdiff@data@max)) )
			message(paste0("Guessed threshold (rounded): ", round(threshold)))
		}
	}
	if(threshold < cdiff@data@min | threshold > cdiff@data@max) warning("Threshold is not within the estimated data range", call. = FALSE)
	
	if(plot) plot(cdiff, main = "Cloud layer: blue - tir difference")
	
	## Thresholding
	if(verbose) message("Begin thresholding")
	cmask <- cdiff > threshold
	cmask <- mask(cmask, cmask, maskvalue = 0)
	
	if(plot) plot(cmask, main = paste0("Cloud mask\nThreshold: ", threshold))
	
	
	## Remove "clouds" smaller than minCloudSize
	if(sanitize) {
		if(verbose) message("Begin sanitzing")
		if(missing(minCloudSize)) minCloudSize <- windowSize1 ^ 2
		w <- matrix(ncol = windowSize1, nrow = windowSize1, 1)
		if(minCloudSize >= windowSize^2) {
			cmod <- focal(cmask, w, na.rm = FALSE)
		} else {
			cmod <- focal(cmask, w, na.rm = TRUE)	
			cmod[cmod < minCloudSize] <- NA		
		}
		cmod[cmod < minCloudSize] <- NA
		cmod[!is.na(cmod)] <- 1L
		if(plot) plot(cmod, main = "Sanitized cloud mask")
		
	}
	
	## Buffer cloud centers (we could also do a circular buffer, but for now this should suffice)
	if(maskGrowing){
		if(verbose) message("Begin region-growing")
		w <- matrix(ncol = windowSize2, nrow = windowSize2, 1)
		cmod <- focal(cmod, w, na.rm = TRUE )
		cmod[!is.na(cmod)] <- 1L
		if(plot) plot(cmod, main = "Region-grown cloud mask")
		
	}
	
	if(plot){
		plotRGB(x, 1, 2, 3, title = "Final mask", stretch = "lin")
		plot(cmod,  legend = FALSE, add = T, col = "yellow")
	}
	
	## Reset par
	par(op)
	
	## Return
	names(cmod) <- "CMASK"
	if(returnDiffLayer) cmod <- stack(cmod, cdiff)
	return(cmod)	
}