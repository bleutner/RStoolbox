#' Simple Cloud Detection
#' 
#' Developed for use with Landsat data \code{cloudMask} relies on the distinctive difference between the blue (or any other short-wave band) and thermal band for semi-automated creation of a cloud mask.
#' 
#' @param x RasterBrick or RasterStack with reflectance and brightness temperature OR the mask of a previous run of \code{cloudMask} with \code{returnDiffLayer=TRUE}. 
#' @param threshold cloud detection threshold. If not provided it will be guessed. Everything *below* this threshold will be considered a cloud pixel (unless it is removed by filtering afterwards).
#' @param blue bandname or number for the blue band
#' @param tir bandname or number for the thermal band
#' @param buffer Integer (number of pixels). If given, a buffer will be added to the identified cloud centers.
#' @param plot logical. Plots of the cloud mask for all sub-steps (sanitizing etc.) Helpful to find proper parametrization.
#' @param verbose logical. Print messages or suppress.
#' @note Typically clouds are cold in the thermal region and have high reflectance in short wavelengths (blue). By calculating a normalized difference index between the two bands and thresholding a rough cloud mask can be obtained.
#' Before calculating the normalized difference thermal cloud index (NDTCI) the thermal band will be matched to the same value range as the blue band. Therefore, it doesn't matter whether you
#' provide DN, radiance or brightness temperature.
#' 
#' This approach to cloud masking is very simplistic. And aims only at rough removal of potentially clouded areas. Nevertheless, it is able to provide satisfactory results. 
#' More sophisticated approaches, including cloud cast shadow detection can be found elsewhere, e.g. \href{http://code.google.com/p/fmask}{fmask}.
#' 
#' It can make sense to find a suitable threshold on a cropped version of the scene. Also make sure you make use of the \code{returnDiffLayer} argument to save yourself one processing step.
#' Buffering should be seen as final polishing, i.e. as long as the pure cloud centers are not detected properly, you might want to turn it off. since it takes some time to calculate.
#' Once your mask detects obvious cloud pixels properly re-enable buffering for fine tuning if desired. Finally, once a suitable threshold is established re-run cloudMask on the whole scene with this threshold and go get a coffee.
#' @export
#' @seealso 
#' \link[raster]{mask}
#' @examples 
#' \dontrun{
#' ls <- stackMeta("path/to/MTL.txt")
#' ls_cor <- radCor(ls, "path/to/MTL.txt") 
#' ls_cmask <-cloudMask(ls_cor, returnDiffLayer = TRUE)  
#' }
cloudMask <- function(x, threshold = 0.8,  blue = "B1_sre", tir = "B6_sre",  buffer = NULL, plot = FALSE, verbose){
	
    if(!missing("verbose")) .initVerbose(verbose)
	## Set-up graphics device 
	op <- par(mfrow = c(2+is.null(buffer),1))
    on.exit(par(op), add = TRUE)
    
	## Calculate or re-reuse cloud difference layer	
	if("NDTCI" %in% names(x)) {
		.vMessage("Re-using NDTCI layer from previous run.") 
		ndtci <- x[["NDTCI"]]
	} else {
        tirn <- normImage(x[[tir]], x[[blue]])
        ndtci <- overlay(stack(tirn, x[[blue]]), fun = function(high, low) (high - low) / (high + low))
		names(ndtci) <- "NDTCI" 
	}
	if(plot) plot(ndtci, main = "Normalized difference thermal cloud index")
	
	## Thresholding
	.vMessage("Begin thresholding")
	cmask <- ndtci < threshold
	cmask <- mask(cmask, cmask, maskvalue = 0)
	
	if(plot) plot(cmask, main = paste0("Cloud mask\nThreshold: ", threshold))
	
	## Buffer cloud centers (we could also do a circular buffer, but for now this should suffice)
	if(!is.null(buffer)){
        # TODO: Check buffer vs. focal  performance
#		w <- matrix(ncol = windowSize2, nrow = windowSize2, 1)
#		cmask <- focal(cmask, w, na.rm = TRUE )
#		cmask[!is.na(cmask)] <- 1L
        .vMessage("Begin region-growing")
        buffer <- buffer * res(cmask)[1]
        cmask <- buffer(cmask, buffer, na.rm = TRUE )
        if(plot) plot(cmask, main = "Region-grown cloud mask")       
    }
    
	if(plot){
		plotRGB(x, 1, 2, 3, stretch = "lin")
		plot(cmask,  legend = FALSE, add = T, col = "yellow")
	}
		 	
	## Return
	names(cmask) <- "CMASK"
	return(stack(cmask, ndtci))
}
