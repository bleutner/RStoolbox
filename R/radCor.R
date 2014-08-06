#' Radiometric calibration and correction
#' 
#' Implements several different methods for absolute radiometric correction of Landsat data.
#' You can either specify a metadata file, or supply all neccesary values manually. With proper parametrization APREF and SDOS should work for other sensors as well.
#' 
#' @param x raster object
#' @param metaData either the result of \code{readMeta} or a path to the meta data (MCL) file. 
#' @param reflectance logical. If \code{TRUE} output will be reflectance, if \code{FALSE} it will be radiance
#' @param bandSet numeric or character. original Landsat band numbers or names in the form of ("B1", "B2" etc). If set to 'full' all bands in the solar region will be processed.
#' @param gain Band-specific sensor gain. Require either gain and offset or Grescale and Brescale to convert DN to radiance.
#' @param offset Band-specific sensor offset. Require either gain and offset or Grescale and Brescale to convert DN to radiance.
#' @param Grescale Band-specific sensor Grescale (gain). Require either gain and offset or Grescale and Brescale to convert DN to radiance.
#' @param Brescale Band-specific sensor Brescale (bias). Require either gain and offset or Grescale and Brescale to convert DN to radiance.
#' @param sunElev Sun elevation in degrees
#' @param satZenith Satellite sensor zenith angle (0 for Landsat)
#' @param edist Earth-Sun distance in AU.
#' @param esun Mean exo-atmospheric solar irradiance, as given by Chandler et al. 2009 or others.
#' @param Lhaze Haze value, such as SHV from DOS() function. Not needed for apparent reflectance.
#' @param Radiometric correction method to be used. There are currently four methods available:
#'  "APREF", "DOS" (Chavez 1989), "COSTZ" (Chavez 1996).
#' @note This is a fork of randcorr in the landsat package. It may be slower, however it works on Raster* objects and hence is memory-safe.  
#' @references S. Goslee (2011): Analyzing Remote Sensing Data in R: The landsat Package. Journal of Statistical Software 43(4).
#' @export
#' @seealso \link[landsat]{radiocorr} 
radCor <-	function(x, metaData, reflectance = TRUE, satellite, bandSet = "full", gain, offset, G_rescale, B_rescale,
		sunElev, satZenith = 0, edist, esun, date, SHV, hazeBand, atHaze,  method = "APREF"){
	# DISCUSS: Should we apply for a "processing level" slot in raster?
	# http://landsat.usgs.gov/Landsat8_Using_Product.php
	
	if(!method %in% c("APREF", "DOS", "COSTZ", "SDOS")) stop("method must be one of 'APREF', 'DOS', 'COSTZ' 'SDOS'", call.=FALSE)
	
	if(!missing(metaData)) {
		
		## Read metadata from file
		if(is.character(metaData)) metaData <- readMeta(metaData)
		
		satellite 	<- metaData$UNIFIED_METADATA$SPACECRAFT_ID
		sensor 		<- metaData$UNIFIED_METADATA$SENSOR_ID
		offset 		<- metaData$UNIFIED_METADATA$RAD_OFFSET
		gain 		<- metaData$UNIFIED_METADATA$RAD_GAIN
		edist		<- metaData$UNIFIED_METADATA$EARTH_SUN_DISTANCE
		sunElev		<- metaData$UNIFIED_METADATA$SUN_ELEVATION
		rad 		<- metaData$UNIFIED_METADATA$RADIOMETRIC_RES
		
	} else {
		### HARD CODED > FIX!!
		sensor = 1
		rad = 8
		###
		if(missing(offset) | missing(gain)) {
			if(missing(G_rescale) | missing(B_rescale)) { stop("Please specify either a) metaData, b) gain and offset, c) B_rescale and G_rescale", call. = FALSE )
			} else {
				offset 	<- -1 * B_rescale / G_rescale  
				gain 	<-  1 / G_rescale
			}
		}
		
		
		if(missing(edist)) {
			if(missing(date)) { 
				stop("Please specify either a) edist or b)date", call. = FALSE) 
			} else {
				edist <- ESdist(date) 
			}
		}
	}
	satZenith	<- satZenith * pi / 180
	satphi 		<- cos(satZenith)
	suntheta 	<- cos((90 - sunElev) * pi / 180)	
	
	## Query internal db	
	sDB <- LANDSAT.db[[satellite]][[sensor]]
	
	## We use getNumeric to deal with band name appendices (e.g. LS7 can have to versions of band 6: B6_VCID_1 and B6_VCID_2
	## which would not match the database name B6
	sDB 	<- sDB[match(paste0("B", sapply(getNumeric(names(x)),"[",1)), sDB$band),]	
	sDB		<- sDB[match(sDB$band, paste0("B",sapply(getNumeric(names(x)),"[",1))),]
	
	if(any(bandSet == "full")) {
		bandSet <- names(x)
	} else {
		if(is.numeric(bandSet)) bandSet <- paste0("B", bandSet)
	}	
	corBands <- sDB[!sDB$bandtype %in% c("TIR", "PAN"), "band"]
	bandSet 	<- bandSet[bandSet %in% corBands]
	exclBands <- names(x)[!names(x) %in% bandSet]
	
	original_bands <- names(x)  
	
	if(missing(esun)) {
		esun <- sDB[,"esun"] 
		names(esun) <- sDB$band
	}
	
	if(missing(metaData))	names(offset) <- names(gain) <- bandSet
	
	
	## Create subset with rasters to process and rasters not to process
	if(any(!names(x) %in% corBands)) {
		message("x contains thermal and/or panchromatic bands, which will not be processed")	
		xna <- x[[which(!names(x) %in% bandSet)]] 
	}
	x <- x[[bandSet]]
	
	message("Processing bands: ", paste(bandSet, collapse = ", "))
	if(length(exclBands) > 0) message("Excluding bands: ", paste(exclBands, collapse = ", "))
	
	
	if(method == "APREF") {
		TAUz <- 1
		TAUv <- 1
		Edown <- 0
		Lhaze <- 0
	} else {
		
		if(missing(SHV)){
			if(missing(hazeBand))  hazeBand <- 1
			if(length(hazeBand) > 1) {
				warning("Automatic search for SHV values is intended for one band only. For more bands please estimate hzae DNs manually using estimateSHV() \nhazeBand was automatically reset to 1")
				hazeBand <- 1 }
			message("SHV was not provided -> Estimating SHV automatically")
			dP <- 0.02
			## We suppress warnings because we search for a possible value autimatically in case we missed the first time
			SHV <- suppressWarnings(estimateSHV(x, hazeBand = hazeBand, darkProp = dP , plot = FALSE, returnTables = TRUE))
			while(is.na(SHV[[1]])){
				dP <- dP * 0.9
				SHV <- suppressWarnings(estimateSHV(SHV, hazeBand = hazeBand, darkProp = dP, plot = T, returnTables = TRUE))
			}
			message(paste0("SHV estimated as: ", SHV[[1]]))
			SHV <- SHV[[1]]
		}
		
		
		# For SDOS gain, offset, Lhaze and Esun must be provided as coresponding vectors of equal length
		if(method == "SDOS"){
			hazeBand <- bandSet
		} 
		
		## 1% correction and conversion to radiance
		SHV  <- SHV - gain[hazeBand] * 0.01 * esun[hazeBand] / edist ^ 2 * suntheta / pi
		SHV	 <- SHV - offset[hazeBand]
		
		if(method %in% c("DOS", "COSTZ")) {
			
			TAUz <- 1
			TAUv <- 1
			Edown <- 0
			
			if (method == "COSTZ") {
				TAUz <- suntheta
				TAUv <- satphi
			} 
			
			## Pick atmoshpere type
			if(missing(atHaze)) {
				atHaze.db <- data.frame(min = c(1,56,76,96,116), max = c(55,75,95,115,255)) / 255 * (2^rad-1)
				atHaze <- c("veryClear", "clear", "moderate", "hazy", "veryHazy")[SHV > atHaze.db[,1] & SHV <= atHaze.db[,2]]
				message("Selcting atmosphere: '", atHaze, "'")
			}		
			SHV	  <- SHV * 	sDB[match(bandSet,sDB$band), paste0(hazeBand,"_", atHaze)]
			
			## Calculate corrected RAD_haze
			NORM <- gain[bandSet] / gain[hazeBand]
			Lhaze <- SHV * NORM + offset[bandSet]	
		}
		# In case Lhaze becomes negative we reset it to zero to prevent artefacts.
		Lhaze [Lhaze < 0] <- 0
		
		
	}
	
	offset	<- offset[bandSet]
	gain 	<- gain[bandSet]
	esun <- esun[bandSet]
	
	if(satellite != "LANDSAT8"){
		
		if(!reflectance) {
			## TOA Radiance
			x <-  (gain * x + offset) / suntheta
		} else {
			## At-surface reflectance (precalculate coefficients to speed up raster processing)
			a <- (pi * edist ^ 2)/(TAUv * (esun * suntheta * TAUz + Edown) * gain)	
			b <- a * (- Lhaze - offset)
			x <- a * x  + b
		}
		
	} else {
		
		if(reflectance) {
			offset 		<- metaData$UNIFIED_METADATA$REF_OFFSET
			gain 		<- metaData$UNIFIED_METADATA$REF_GAIN
		} else {
			offset 		<- metaData$UNIFIED_METADATA$RAD_OFFSET
			gain 		<- metaData$UNIFIED_METADATA$RAD_GAIN
		}
		## At sensor radiance / reflectance
		x <-  (gain * x + offset) / suntheta
		
		## At-surface reflectance?
		
	}
	
	
	if(any(!original_bands %in% bandSet)) {
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
						centerWavl = c(0.485, 0.560, 0.660, 0.835, 1.650,11.335,2.220,0.706),
						esun = c(1997,1812,1533,1039,230.8,NA,84.9,1362)
				)
		),
		LANDSAT8 = list(
				OLI_TIRS = data.frame(band = c(paste0("B",1:11), "BQA"),
						bandtype = c(rep("REF", 7), "PAN", "REF", "TIR", "TIR", "QA"),
						spatRes1 = c(rep(30, 7), 15, rep(30,4)),
						spatRes2 = c(rep(30, 7), 15, rep(30,4)),  ## ETM+ Band 6 is acquired at 60-meter resolution. Products processed after February 25, 2010 are resampled to 30-meter pixels.
						centerWavl = c(0.44,0.48,0.56,0.655,0.865,1.61,2.2,0.59,1.37,10.6,11.5, NA), 
						esun = c(NA, 2067, 1893, 1603, 972.6, 245, 79.72, NA, 399.7, NA, NA, NA ) ## http://www.gisagmaps.com/landsat-8-atco/ ##http://landsat.usgs.gov/Landsat8_Using_Product.php
				)
		)

) 

exponents <- c(-4, -2, -1, -.7, -.5)
for(s in names(LANDSAT.db)){
	bandType		<- LANDSAT.db[[s]][[1]][,"bandtype"] == "REF"
	centerWavl		<- LANDSAT.db[[s]][[1]][bandType, "centerWavl"] 
	bands 			<- LANDSAT.db[[s]][[1]][bandType, "band"]
	
	## Calc Chavez Tab 1
	TAB1			<- sapply(exponents, function(x) centerWavl ^ x)
	rownames(TAB1)  <- bands
	colnames(TAB1)	<- c("veryClear", "clear", "moderate", "hazy", "veryHazy")
	
	## Calc Chavez Tab 2, but only until SHVB = B4, larger wavelengths don't make sense to estimate haze
	TAB2 <- lapply(paste0("B", 1:4), function(SHVB){ sweep(TAB1, 2, TAB1[SHVB,], "/")})
	TAB2 <- do.call("cbind", TAB2)
	colnames(TAB2) <- paste0(rep(paste0("B", 1:4), each = 5),"_", colnames(TAB2))
	
	LANDSAT.db[[s]][[1]] <-  merge(LANDSAT.db[[s]][[1]] , TAB2, by.x = "band", by.y = "row.names", all.x = TRUE, sort = FALSE)
}







