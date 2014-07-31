#' Radiometric calibration and correction for Landsat data
#' 
#' Implements several different methods for absolute radiometric correction of Landsat data.
#' You can either specify a metadata file, or supply all neccesary values manually.
#' 
#' @param x raster object
#' @param metaData either the result of \code{readMeta} or a path to the meta data (MCL) file. 
#' @param reflectance logical. If \code{TRUE} output will be reflectance, if \code{FALSE} it will be radiance
#' @param bandSet numeric or character. original Landsat band numbers or names in the form of ("B1", "B2" etc). If set to 'full' all bands in the solar region will be processed.
#' @param gain Band-specific sensor gain. Require either gain and offset or Grescale and Brescale to convert DN to radiance.
#' @param offset Band-specific sensor offset. Require either gain and offset or Grescale and Brescale to convert DN to radiance.
#' @param Grescale Band-specific sensor Grescale (gain). Require either gain and offset or Grescale and Brescale to convert DN to radiance.
#' @param Brescale Band-specific sensor Brescale (bias). Require either gain and offset or Grescale and Brescale to convert DN to radiance.
#' @param sunelev Sun elevation in degrees
#' @param satzenith Satellite sensor zenith angle (0 for Landsat)
#' @param edist Earth-Sun distance in AU.
#' @param Esun Mean exo-atmospheric solar irradiance, as given by Chandler et al. 2009 or others.
#' @param Lhaze Haze value, such as SHV from DOS() function. Not needed for apparent reflectance.
#' @param Radiometric correction method to be used. There are currently four methods available:
#'  "apparentreflectance", "DOS" (Chavez 1989), "COSTZ" (Chavez 1996), "DOS4" (SWS+2001).
#' @note This is a fork of randcorr in the landsat package, it differs mainly in that it works on raster objects and hence is memory-safe.  
#' @references S. Goslee (2011): Analyzing Remote Sensing Data in R: The landsat Package. Journal of Statistical Software 43(4).
#' @export
#' @seealso \link[landsat]{radiocorr}
radCor <-	function(x, metaData, reflectance = TRUE, satellite, bandSet = "full", gain, offset, G_rescale, B_rescale,
		sunelev, satzenith = 0, edist, Esun, date, Lhaze, method = "apparentreflectance"){
	# DISCUSS: Should we apply for a "processing level" slot in raster?
	#http://landsat.usgs.gov/Landsat8_Using_Product.php
	
	if(!method %in% c("apparentreflectance", "DOS", "COSTZ", "DOS4")) stop("method must be one of 'apparentreflectance', 'DOS', 'COSTZ', 'DOS4' ", call.=FALSE)
	
	if(!missing(metaData)) {
		
		## Read metadata from file
		if(is.character(metaData)) metaData <- readMeta(metaData)
		
		satellite 	<- metaData$UNIFIED_METADATA$SPACECRAFT_ID
		sensor 		<- metaData$UNIFIED_METADATA$SENSOR_ID
		offset 		<- metaData$UNIFIED_METADATA$RAD_OFFSET
		gain 		<- metaData$UNIFIED_METADATA$RAD_GAIN
		edist		<- metaData$UNIFIED_METADATA$EARTH_SUN_DISTANCE
		sunelev		<- metaData$UNIFIED_METADATA$SUN_ELEVATION
		
	} else {
		
		if(missing(offset) | missing(gain)) {
			if(missing(G_rescale) | missing(B_rescale)) { stop("Please specify either a) metaData, b) gain and offset, c) B_rescale and G_rescale", call. = FALSE )
			} else {
				offset 	<- -1 * B_rescale / G_rescale  
				gain 	<- 1 / G_rescale
			}
		}
		if(missing(edist)) {
			if(missing(date)) stop("Please specify either a) edist or b)date", call. = FALSE)
		} else {
			edist <- ESdist(date) 
		}
		
	}
	satzenith	<- satzenith * pi / 180
	satphi 		<- cos(satzenith)
	suntheta 	<- cos((90 - sunelev) * pi / 180)	
	
	
	if(method == "apparentreflectance") {
		TAUz <- 1
		TAUv <- 1
		Edown <- 0
		Lhaze <- 0
	} 
#	if(method == "DOS") {
#		TAUz <- 1
#		TAUv <- 1
#		Edown <- 0
#		if(missing(Lhaze)) stop("This model requires Lhaze to be specified.\n")	
#	}
#	if(method == "COSTZ") {
#		TAUz <- suntheta
#		TAUv <- satphi
#		Edown <- 0
#		if(missing(Lhaze)) stop("This model requires Lhaze to be specified.\n")
#	} 
#	if(method == "DOS4") {
#		TAUv <- TAUz <- 1
#		taudiff <- 1
#		tau <- 9999
#		Edown <- 0
#		
#		Lhaze.orig <- Lhaze
#		
#		while(abs(taudiff) > 0.0000001) {
#			taudiff <- tau
#			
#			## if Lhaze is too large, the formula tries to take log of a negative number
#			## iteratively adjust Lhaze downward until it works
#			## This is a lazy kludge!!!
#			
#			Eo <- Esun / edist ^ 2
#			
#			Lp <- (Lhaze - offset) / gain - 0.01 * (Eo * suntheta * TAUz + Edown) * TAUv / pi
#			
#			taustep <- 1 - (4 * pi * Lp) / (Eo * suntheta)
#			
#			while(taustep < 0) {
#				Lhaze <- Lhaze - 1
#				Lp <- (Lhaze - offset) / gain - 0.01 * (Eo * suntheta * TAUz + Edown) * TAUv / pi
#				taustep <- 1 - (4 * pi * Lp) / (Eo * suntheta)
#			}
#			
#			tau <- -1 * suntheta * log(1 - (4 * pi * Lp) / (Eo * suntheta))
#			TAUv <- exp(-1 * tau / satphi)
#			TAUz <- exp(-1 * tau / suntheta)		
#			Edown <- pi * Lp
#			
#			taudiff <- taudiff - tau
#			
#		}
#		
#		if(!identical(Lhaze.orig, Lhaze)) warning(paste("Lhaze adjusted from ", Lhaze.orig, " to ", Lhaze, sep=""))
#		
#		if(missing(Lhaze)) stop("This model requires Lhaze to be specified.\n")
#		
#		
#	}
#	
	## If not the full set is used, make sure we pick the right parameters
	
	if(any(bandSet == "full")) {
		bandSet <- names(x)
	} else {
		if(is.numeric(bandSet)) bandSet <- paste0("B", bandSet)
		
		Lhaze 	<- Lhaze[bandSet]
		offset	<- offset[bandSet]
		gain 	<- gain[bandSet]
	}  
	
	
	## Query internal db	
	sDB <- LANDSAT.db[[satellite]][[sensor]]
	
	## We use getNumeric to deal with band name appendices (e.g. LS7 can have to versions of band 6: B6_VCID_1 and B6_VCID_2
	## which would not match the database name B6
	sDB <- sDB[match(paste0("B", getNumeric(names(x))[1,]), sDB$band),]
	
	sDB <- sDB[match(sDB$band, paste0("B",getNumeric(names(x))[,1])),]
	
	if(missing(Esun)) Esun <- sDB[,"esun"]
	
	ignore <- sDB$bandtype %in% c("TIR")
	original_bands <- names(x)  
	
	## Create subset with rasters to process and rasters not to process
	if(any(ignore)) {
		message("x contains TIR bands, which will not be processed")	
		xna <- x[[which(ignore)]] 
	}
	x <- x[[which(!ignore)]]
	
	message("Processing bands: ", paste(bandSet[!ignore], collapse = ", "))
	if(any(ignore)) message("Excluding bands: ", paste(bandSet[ignore], collapse = ", "))
	
	if(satellite != "LANDSAT8"){
		## Haze correction	
		## Lhaze output from DOS() is in DN, so this is done as a separate step
		if(Lhaze != 0) x <- x - Lhaze
		
		## At-sensor radiance	
		x <- (x - offset[bandSet[!ignore]]) / gain[bandSet[!ignore]]
		
		## At-surface reflectance	
		if(reflectance) x <-  (pi * edist^2 * x) / (TAUv * (Esun[!ignore] * suntheta * TAUz + Edown))	
		
	} else {
		if(reflectance) {
			offset 		<- metaData$UNIFIED_METADATA$REF_OFFSET
			gain 		<- metaData$UNIFIED_METADATA$REF_GAIN
		} else {
			offset 		<- metaData$UNIFIED_METADATA$RAD_OFFSET
			gain 		<- metaData$UNIFIED_METADATA$RAD_GAIN
		}
		## At sensor reflectance
		x <-  (gain * x + offset) / suntheta
		
		## At-surface reflectance?
		
	}
	
	if(any(ignore)) {
		x <- stack(x,xna)
		x <- x[[original_bands]]
	}
	
	
	return(x)
}


#' Landsat auxilliary data. Taken from Chander et al 2009
#' spatRes resampling: http://landsat.usgs.gov/band_designations_landsat_satellites.php
LANDSAT.db <- list(
		LANDSAT5 = list (
				TM = data.frame(band = paste0("B", 1:7),
						bandtype = c(rep("REF", 5), "TIR", "REF"),
						centerWavl = c(0.485, 0.569, 0.66, 0.840, 1.676, 11.435, 2.223),
						spatRes1 = rep(30, 7),
						spatRes2 = c(rep(30,5), 60, 30), ## TM Band 6 was acquired at 120-meter resolution, but products processed before February 25, 2010 are resampled to 60-meter pixels. Products processed after February 25, 2010 are resampled to 30-meter pixels.
						esun = c(1983, 1796, 1536, 1031, 220, NA, 83.44))
		),
		LANDSAT7 = list(
				ETM = data.frame(band = paste0("B",1:8),
						bandtype = c(rep("REF", 5), "TIR", "REF", "PAN"),
						spatRes1 = c(rep(30, 7), 15),
						spatRes2 = c(rep(30,5), 60, 30, 15),  ## ETM+ Band 6 is acquired at 60-meter resolution. Products processed after February 25, 2010 are resampled to 30-meter pixels.
						centerWavl = c(0.483,0.56,0.662,0.825,1.648,11.335,2.206,0.706),
						esun = c(1997,1812,1533,1039,230.8,NA,84.9,1362)
				)
		),
		LANDSAT8 = list(
				OLI_TIRS = data.frame(band = c(paste0("B",1:11), "BQA"),
						bandtype = c(rep("REF", 7), "PAN", "REF", "TIR", "TIR", "QA"),
						spatRes1 = c(rep(30, 7), 15, rep(30,4)),
						spatRes2 = c(rep(30, 7), 15, rep(30,4)),  ## ETM+ Band 6 is acquired at 60-meter resolution. Products processed after February 25, 2010 are resampled to 30-meter pixels.
						centerWavl = c(0.44,0.48,0.56,0.655,0.865,1.61,2.2,0.59,1.37,10.6,11.5, NA), 
						esun = rep(NA,12)##http://landsat.usgs.gov/Landsat8_Using_Product.php
				)
		)

)


