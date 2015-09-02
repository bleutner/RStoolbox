#' Simple Cloud Detection
#' 
#' Developed for use with Landsat data \code{cloudMask} relies on the distinctive difference between the blue (or any other short-wave band) 
#' and thermal band for semi-automated creation of a cloud mask. Since it relies on thermal information it doesn't work well for sensors without
#' thermal bands. 
#' 
#' @param x RasterBrick or RasterStack with reflectance and brightness temperature OR the mask of a previous run of \code{cloudMask} with \code{returnDiffLayer=TRUE}. 
#' @param threshold Numeric. cloud detection threshold. If not provided it will be guessed. Everything *below* this threshold will be considered a cloud pixel (unless it is removed by filtering afterwards).
#' @param blue Character or integer. Bandname or number for the blue band
#' @param tir Character or integer. Bandname or number for the thermal band
#' @param buffer Integer. Number of pixels to use as a buffer that will be added to the identified cloud centers.
#' @param plot Logical. Plots of the cloud mask for all sub-steps (sanitizing etc.) Helpful to find proper parametrization.
#' @param verbose Logical. Print messages or suppress.
#' @note Typically clouds are cold in the thermal region and have high reflectance in short wavelengths (blue). By calculating a normalized difference index between the two bands and thresholding a rough cloud mask can be obtained.
#' Before calculating the spectral cloud index (let's call it Normalized Difference Thermal Cloud Index (NDTCI)) the thermal band will be matched to the same value range as the blue band. Therefore, it doesn't matter whether you
#' provide DN, radiance or brightness temperature.
#' 
#' This approach to cloud masking is very simplistic. And aims only at rough removal of potentially clouded areas. Nevertheless, it is able to provide satisfactory results. 
#' More sophisticated approaches, including cloud cast shadow detection can be found elsewhere, e.g. \href{http://code.google.com/p/fmask}{fmask}.
#' 
#' It can make sense to find a suitable threshold on a cropped version of the scene. Also make sure you make use of the \code{returnDiffLayer} argument to save yourself one processing step.
#' Buffering should be seen as final polishing, i.e. as long as the pure cloud centers are not detected properly, you might want to turn it off. since it takes some time to calculate.
#' Once your mask detects obvious cloud pixels properly re-enable buffering for fine tuning if desired. Finally, once a suitable threshold is established re-run cloudMask on the whole scene with this threshold and go get a coffee.
#' @export
#' @return
#' Returns a RasterStack with two layers: CMASK contains the binary cloud mask (1 = cloud, NA = not-cloud) and NDTCI contains the cloud index.
#' @seealso 
#' \code{\link{cloudShadowMask}}
#' @template examples_cloudMask
cloudMask <- function(x, threshold = 0.8,  blue = "B1_sre", tir = "B6_sre", buffer = NULL, plot = FALSE, verbose){
    
    if(!missing("verbose")) .initVerbose(verbose)
    ## Set-up graphics device 
    op <- par(mfrow = c(2+is.null(buffer),1))
    on.exit(par(op), add = TRUE)
    
    ## Calculate or re-reuse cloud difference layer	
    if("NDTCI" %in% names(x)) {
        .vMessage("Re-using NDTCI layer from previous run.") 
        ndtci <- x[["NDTCI"]]
    } else {
        tirn <- rescaleImage(x[[tir]], x[[blue]])
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
        .vMessage("Begin region-growing")
        bufferMethod <- 1
        if(bufferMethod){
            # TODO: Check buffer vs. focal  performance
            w <- matrix(ncol = buffer, nrow = buffer, 1)
            w[c(1,buffer,1,buffer), c(1,1,buffer,buffer)] <- 0
            cmask <- focal(cmask, w, na.rm = TRUE )
            cmask[!is.na(cmask)] <- 1L
        } else {
            buffer <- buffer * res(cmask)[1]
            cmask <- buffer(cmask, buffer, na.rm = TRUE )
            if(plot) plot(cmask, main = "Region-grown cloud mask")
        }
    }
    
    if(plot){
        plotRGB(x, 1, 2, 3, stretch = "lin")
        plot(cmask,  legend = FALSE, add = T, col = "yellow")
    }
    
    ## Return
    names(cmask) <- "CMASK"
    return(stack(cmask, ndtci))
}


#' Cloud Shadow Masking for Flat Terrain
#' 
#' Intended for interactive use, \code{cloudShadowMask} will ask the user to select a few 
#' corresponding cloud/cloudShadow pixels which will be used to estimate coordinates 
#' for a linear cloudmask shift.
#' 
#' @param img Raster* object containing the scene 
#' @param cm Raster* object. Cloud mask (typically the result of \code{\link{cloudMask}})
#' @param nc Integer. Number of control points. A few points (default) are fine because the final shift is estimated by \link{coregisterImages}.
#' @param shiftEstimate NULL or numeric vector of length two (x,y). Estimated displacement of shadows in map units. If \code{NULL}, the user will be asked to select control points interactively.
#' @param preciseShift NULL or numeric vector of length two (x,y). Use this if cloud/cloud-shadow displacement is already known, e.g. from a previous run of \code{cloudShadowMask}.
#' @param quantile Numeric (between 0 and 1). Quantile threshold used for image co-registration. By default the 20\% quantile of the total intensity (sum) of the image is used as potential shadow mask.
#' @param returnShift Logical. Return a numeric vector containing the shift parameters. Useful if you estimate parameters on a subset of the image.
#' @details 
#' This is a very simplistic approach to cloud shadow masking (simple shift of the cloud mask). It is not image based and accuracy will suffer from clouds at different altitudes. However, just as cloudMask
#' this is a quick and easy to use tool for Landsat data if you're just working on a few scenes and don't have fMask or CDR data at hand. Although for some test scenes
#' it does perform surprisingly well.
#' @return 
#' Returns a RasterLayer with the cloud shadow mask (0 = shadow, NA = not-shadow).
#' @seealso \link{cloudMask}
#' @export
#' @template examples_cloudMask
cloudShadowMask <- function (img, cm, nc = 5, shiftEstimate = NULL, preciseShift = NULL, quantile = 0.2, returnShift = FALSE) {
    
    stopifnot(quantile > 0 & quantile < 1) 
    #csind <- overlay(stack(tir, x[[]]), fun = function(high, low) (high - low) / (high + low))
    if(is.null(preciseShift)){ 
        
        csind <- sum(img)
        csindm <- csind < quantile(csind, quantile)
        if(is.null(shiftEstimate)){
            
            plotRGB(img, stretch = "hist")
            plot(cm[[1]], add = T, legend = FALSE)
            message("Please select cloud and corresponding shadow pixels (alternating cloud,shadow,cloud,shadow...)\n")
            sel <- click(cm[[1]], n = nc*2, xy = TRUE, show = FALSE, pch = 13)
            
            shiftEstimate <- colMeans(apply(sel[,1:2], 2, diff)[seq(1,nrow(sel),2),])   
            shiftEstimate <- shiftEstimate / res(img)[1] 
        } else if(!(is.vector(shiftEstimate) && length(shiftEstimate) == 2) ) {
            stop("shiftEstimat must be a numeric vector of length two (x,y) or NULL")
        }
        
        cma <- is.na(cm[[1]])
        shifts <- matrix(shiftEstimate + rep(seq(-3,3,0.5), each = 2), ncol=2, byrow=TRUE)
        colnames(shifts) <- c("x", "y")
        cms <- coregisterImages(cma, csindm, shift = shifts, nBins = 2, nSamples = ncell(cma), reportStats = TRUE)
        shiftPar <- cms$bestShift
        cms <- cms$coregImg
        cms[cms == 1] <- NA
    } else {
        cms <- shift(cm[[1]], preciseShift[1], preciseShift[2])
    }
    cms <- resample(cms, cm, method = "ngb")
    if(returnShift) return(list(shift = shiftPar, shadowMap = cms)) else return(cms)
    
}


