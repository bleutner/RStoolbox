#' Change Vector Analysis
#' 
#' Calculates angle and magnitude of change vectors. 
#' Dimensionality is limited to two bands per image. 
#' 
#' @param x RasterBrick or RasterStack or SpatRaster with two layers. This will be the reference/origin for the change calculations. Both rasters (y and y) need to correspond to each other, i.e. same resolution, extent and origin.
#' @param y RasterBrick or RasterStack or SpatRaster with two layers. Both rasters (y and y) need to correspond to each other, i.e. same resolution, extent and origin.
#' @param tmf Numeric. Threshold median factor (optional). Used to calculate a threshold magnitude for which pixels are considered stable, i.e. no change. Calculated as \code{tmf * mean(magnitude[magnitude > 0])}.
#' @param nct Numeric. No-change threshold (optional). Alternative to \code{tmf}. Sets an absolute threshold. Change magnitudes below \code{nct} are considered stable and set to NA.
#' @param ... further arguments passed to writeRaster
#' @details 
#' Change Vector Analysis (CVA) is used to identify spectral changes between two identical scenes which were acquired at different times. 
#' CVA is limited to two bands per image. For each pixel it calculates the change vector in the two-dimensional spectral space. 
#' For example for a given pixel in image A and B for the red and nir band the change vector is calculated for the coordinate pairs: (red_A | nir_A) and (red_B | nir_B).
#' 
#'  The coordinate system is defined by the order of the input bands: the first band defines the x-axis and the second band the y-axis, respectively.
#'  Angles are returned *in degree* beginning with 0 degrees pointing 'north', i.e. the y-axis, i.e. the second band.
#' 
#'
#' @return 
#' Returns a SpatRaster with two layers: change vector angle and change vector magnitude
#' @export 
#' @examples 
#' library(terra)
#'
#' pca <- rasterPCA(lsat_rs)$map
#' 
#' ## Do change vector analysis 
#' cva <- rasterCVA(pca[[1:2]], pca[[3:4]])
#' cva
#' plot(cva)
rasterCVA <- function(x, y, tmf = NULL, nct = NULL,  ...) {
	x <- .toTerra(x)
	y <- .toTerra(y)

	if(nlyr(x) != 2 | nlyr(y) != 2)
		stop("need two rasters with two layers each")
	
	doClamp <- !is.null(tmf) || !is.null(nct) 
	
	if(!is.null(tmf) && !is.null(nct)) {
		stop("'tmf' and 'nct' are exclusive options, cannot use both.", call. = FALSE)
	}
	
	if(!is.null(tmf)) {
		maxMag <- sqrt(sum((as.numeric(t(terra::global(x, "max", na.rm=T))) - as.numeric(t(terra::global(y, "max", na.rm=T))) )^2))*2
		medianBuckets <- seq(1e-10, maxMag, length.out = 2e5)
		RStoolbox_rasterCVAEnv <- new.env()
		RStoolbox_rasterCVAEnv$medianTable <- 0
	}
	anglefun <- function(values,tmf,...) {
		dif <- values[,3:4] - values[,1:2]
		magnitude <- sqrt(rowSums(dif^2))

		if(!is.null(tmf)) {
			RStoolbox_rasterCVAEnv$medianTable <- RStoolbox_rasterCVAEnv$medianTable + table(cut(magnitude, medianBuckets), useNA = "no")
		}

		angle  <- rep(0, length(magnitude))
		sel    <- !is.na(magnitude)
		angle[sel]  <- atan2(dif[sel,1],dif[sel,2]) / pi * 180
		negang <- angle < 0
		angle[negang] <- 360 + angle[negang]
		angle[!sel] <- NA
		cbind(angle, magnitude)
	}

	X    <- c(x,y)
	out <- rast(x, 2)

	names(out) <- c("angle", "magnitude")
	ellips <- list(...)
	
	if(.canProcInMem(X, 2) ) {
		out[] <- anglefun(values(X), tmf)
		if(!doClamp && !is.null(ellips$filename)){
			out <- writeRaster(out, ...)
		}
	} else {
		magfile <- if(!is.null(ellips$filename) && !doClamp) filename else rasterTmpFile()
		X   <- readStart(X)
		out <- writeStart(out, filename = magfile, ...)
		tr  <- terra::blocks(out)
		for (i in 1:tr$n) {
			vo <- values(X, row=tr$row[i], nrows=tr$nrows[i])
			vo <- anglefun(vo, tmf)
			out <- writeValues(out, vo, tr$row[i])
		}
		out <- writeStop(out)
		X <- readStop(X)
	}
	
	if(doClamp) {
		if(!is.null(tmf)) {
			ci <- which(cumsum(RStoolbox_rasterCVAEnv$medianTable) > sum(RStoolbox_rasterCVAEnv$medianTable) / 2)[1]
			medianEstimate <- mean(medianBuckets[c(ci,ci+1)])
			nct <- tmf * medianEstimate
			rm(RStoolbox_rasterCVAEnv)
		}
		out <- do.call(clamp, c(list(x = out, lower=nct), ellips))
		
		names(out) <- c("angle", "magnitude")
	}
	
	return(out)
}
