#' Radiometric calibration and correction
#' 
#' Implements several different methods for absolute radiometric correction of Landsat data.
#' You can either specify a metadata file, or supply all neccesary values manually. With proper parametrization APREF and SDOS should work for other sensors as well.
#' 
#' @param x raster object
#' @param metaData either the result of \code{readMeta} or a path to the meta data (MTL) file. 
#' @param reflectance logical. If \code{TRUE} output will be reflectance, if \code{FALSE} it will be radiance
#' @param thermal logical. If \code{TRUE} thermal bands will be converted to brightness temperature (Kelvin).
#' @param satellite character. Required if metaData is not provided. One of LANDSAT8, LANDSAT7, LANDSAT5.
#' @param bandSet numeric or character. original Landsat band numbers or names in the form of ("B1", "B2" etc). If set to 'full' all bands in the solar region will be processed.
#' @param gain Band-specific sensor gain. Require either gain and offset or Grescale and Brescale to convert DN to radiance.
#' @param offset Band-specific sensor offset. Require either gain and offset or Grescale and Brescale to convert DN to radiance.
#' @param G_rescale Band-specific sensor Grescale (gain). Require either gain and offset or Grescale and Brescale to convert DN to radiance.
#' @param B_rescale Band-specific sensor Brescale (bias). Require either gain and offset or Grescale and Brescale to convert DN to radiance.
#' @param sunElev Sun elevation in degrees
#' @param satZenith sensor zenith angle (0 for Landsat)
#' @param d Earth-Sun distance in AU.
#' @param esun Mean exo-atmospheric solar irradiance, as given by Chandler et al. 2009 or others.
#' @param date Date of data aquisition. Ignored if esun or satellite is provided.
#' @param SHV starting haze value, can be estimated using estimateSHV(). if not provided and method is "DOS" or "COSTZ" SHV will be estimated in an automated fashion. Not needed for apparent reflectance.
#' @param hazeBand band from which SHV was estimated.
#' @param atHaze character. Scene haze characteristics. Will be estimated if not expicilty provided. Must be one of \code{"veryClear", "clear", "moderate", "hazy"} or \code{"veryHazy"}.
#' @param method Radiometric correction method to be used. There are currently four methods available (see Details):
#'  "APREF", "DOS" (Chavez 1989), "COSTZ" (Chavez 1996), SDOS.
#' @note This was originally a fork of randcorr in the landsat package. It may be slower, however it works on Raster* objects and hence is memory-safe.
#' @details  \describe{
#' \item{APREF}{Apparent reflectance}
#' \item{DOS}{Dark object subtratction following Chavez (1989)}
#' \item{COSTZ}{Dark object subtraction following Chaves(1996)}
#' \item{SDOS}{Simple dark object subtraction. Classical DOS, Lhaze must be estimated for each band separately.}
#' }
#' @references S. Goslee (2011): Analyzing Remote Sensing Data in R: The landsat Package. Journal of Statistical Software 43(4).
#' @export
#' @seealso \link[landsat]{radiocorr} 
radCor <-	function(x, metaData, reflectance = TRUE, thermal = TRUE, satellite, bandSet = "full", gain, offset, G_rescale, B_rescale,
        sunElev, satZenith = 0, d, esun, date, SHV, hazeBand, atHaze,  method = "APREF"){
    # http://landsat.usgs.gov/Landsat8_Using_Product.php
    
    if(!method %in% c("APREF", "DOS", "COSTZ", "SDOS")) stop("method must be one of 'APREF', 'DOS', 'COSTZ' 'SDOS'", call.=FALSE)
    
    if(!reflectance & method != "APREF"){
        warning("For radiance calculations the 'method' argument is ignored")
        method <- "APREF"
    }
    
    
    if(!missing(metaData)) {
        
        if(!grepl("MTL", metaData)) stop("The metadata file must be the original MTL file") 
        
        ## Read metadata from file
        if(is.character(metaData)) metaData <- readMeta(metaData)
        
       
        satellite 	<- metaData$UNIFIED_METADATA$SPACECRAFT_ID
        sensor 		<- metaData$UNIFIED_METADATA$SENSOR_ID
        B_rescale	<- metaData$UNIFIED_METADATA$RAD_OFFSET
        G_rescale	<- metaData$UNIFIED_METADATA$RAD_GAIN
        d			<- metaData$UNIFIED_METADATA$EARTH_SUN_DISTANCE
        sunElev		<- metaData$UNIFIED_METADATA$SUN_ELEVATION
        rad 		<- metaData$UNIFIED_METADATA$RADIOMETRIC_RES
        K1			<- metaData$UNIFIED_METADATA$K1
        K2			<- metaData$UNIFIED_METADATA$K2
        
        
  } else {
        ###  FIXME: HARD CODED !!
        sensor = 1
        rad = 8
        ###
        if(missing(G_rescale) | missing(B_rescale)){
            if(missing(offset) | missing(gain)) {
                stop("Please specify either a) metaData, b) gain and offset, c) B_rescale and G_rescale", call. = FALSE )
            } else {
                B_rescale <- 1/gain
                G_rescale <- -offset/gain
            }
        }
        
        
        if(missing(d)) {
            if(missing(date)) { 
                stop("Please specify either a) edist or b) date", call. = FALSE) 
            } else {
                d <- .ESdist(date) 
            }
        }
    }
    
    if(satellite == "LANDSAT8" & method != "APREF") {
        warning("DOS, COSTZ and SDOS are currently not implemented for Landsat 8. Using official reflectance calibration coefficients, i.e. output corresponds to method = 'APREF'", call. = FALSE) 
        method <- "APREF"
    }
    
    satZenith	<- satZenith * pi / 180
    satphi 		<- cos(satZenith)
    suntheta 	<- cos((90 - sunElev) * pi / 180)	
    
    ## Query internal db	
    sDB <- .LANDSATdb[[satellite]][[sensor]]
    
    ## LS7 can have to versions of band 6: B6_VCID_1 and B6_VCID_2 which would not match the database name B6
    sDB 	<- sDB[match(gsub("_VCID_[12]", "", names(x)), sDB$band),]	
    sDB		<- sDB[match(sDB$band, gsub("_VCID_[12]", "", names(x))),]
    
    if(any(bandSet == "full")) {
        bandSet <- names(x)
    } else {
        if(is.numeric(bandSet)) bandSet <- names(x)[bandSet]
    }	
    
    if(missing(metaData))	names(B_rescale) <- names(G_rescale) <- bandSet
    
    origBands 	<- names(x)   
    corBands 	<- sDB[!sDB$bandtype %in% c("TIR", "PAN"), "band"]
    bandSet 	<- bandSet[bandSet %in% corBands]
    
    
    if(thermal){
        tirBands	<- if(satellite=="LANDSAT8") c("B10_DN", "B11_DN") else c("B6_DN", "B6_VCID_1_DN", "B6_VCID_2_DN")	
        tirBands 	<- origBands[origBands %in% tirBands]
    } else {
        tirBands <- NULL
    }
    exclBands	<- origBands[!origBands %in% c(bandSet, tirBands)]
    
    if(length(exclBands) > 0) {
        xexc <- x[[exclBands]] 
    } else {
        xexc <- NULL
    }
    
    if(missing(esun)) {
        esun <- sDB[,"esun"] 
        names(esun) <- sDB$band
    }
    xref <- x[[bandSet]]
    
    if(reflectance) {
        message("Bands to convert to reflectance: ", paste(bandSet, collapse = ", "))
        if(length(tirBands) > 0 & thermal) message("Thermal bands to convert to brightness temperatures: ", paste(tirBands, collapse=", "))
        if(length(exclBands) > 0) message("Excluding bands: ", paste(exclBands, collapse = ", "))	
    } else {
        bandSet <- c(bandSet, tirBands)
        message("Bands to convert to toa radiance: ", paste(bandSet, collapse = ", "))
    }
    
    ## Thermal processing
    if(thermal & reflectance & length(tirBands) > 0) {
        message("Processing thermal band(s)")
        ## Convert to radiance
        L <- G_rescale[tirBands] * x[[tirBands]] + B_rescale[tirBands]
        ## Convert to temperature
        xtir <- K2 / log(K1/L + 1) 
        names(xtir) <- gsub("DN", "BT", tirBands)
    } else {
        xtir <- NULL
    }
    
    message("Processing radiance / reflectance")
    
    ## Radiance and reflectance processing
    if(method == "APREF") {
        TAUz <- 1
        TAUv <- 1
        Edown <- 0
        Lhaze <- 0
        
    } else {
        
        ## Estimate SHV automatically
        if(missing(SHV)){
            if(missing(hazeBand))  hazeBand <- 1
            if(length(hazeBand) > 1) {
                warning("Automatic search for SHV values is intended for one band only. For more bands please estimate haze DNs manually using estimateSHV() \nhazeBand was automatically reset to 1")
                hazeBand <- 1 }
            message("SHV was not provided -> Estimating SHV automatically")
            dP <- 0.02
            ## We suppress warnings because we search for a possible value autimatically in case we missed the first time
            SHV <- suppressWarnings(estimateSHV(x, hazeBand = hazeBand, darkProp = dP , plot = FALSE, returnTables = TRUE))
            while(is.na(SHV[[1]])){
                dP	<- dP * 0.9
                SHV <- suppressWarnings(estimateSHV(SHV, hazeBand = hazeBand, darkProp = dP, plot = FALSE, returnTables = TRUE))
            }
            message(paste0("SHV estimated as: ", SHV[[1]]))
            SHV <- SHV[[1]]
        }
        
        
        # For SDOS gain, offset, Lhaze and Esun must be provided as coresponding vectors of equal length
        if(method == "SDOS") hazeBand <- bandSet 
        TAUz <- 1
        TAUv <- 1
        Edown <- 0				
        if (method == "COSTZ") {
            TAUz <- suntheta
            TAUv <- satphi
        }  
        
        ## 1% correction and conversion to radiance
        Ldo <- 0.01 * ((esun[hazeBand] * suntheta * TAUz) + Edown) * TAUv / (pi * d ^ 2)
        Lhaze <- (SHV * G_rescale[hazeBand] + B_rescale[hazeBand]) - Ldo
        
        if(method %in% c("DOS", "COSTZ")) {		
            ## Pick atmoshpere type
            if(missing(atHaze)) {
                atHaze.db <- data.frame(min = c(1,56,76,96,116), max = c(55,75,95,115,255)) / 255 * (2^rad-1)
                atHaze <- c("veryClear", "clear", "moderate", "hazy", "veryHazy")[Lhaze > atHaze.db[,1] & Lhaze <= atHaze.db[,2]]
                message("Selecting atmosphere: '", atHaze, "'")
            }		
            Lhaze	  <- Lhaze  * sDB[match(bandSet,sDB$band), paste0(hazeBand,"_", atHaze)]
            
            ## Calculate corrected RAD_haze
            NORM  <- G_rescale[bandSet] / G_rescale[hazeBand]
            Lhaze <- Lhaze * NORM + B_rescale[bandSet]	
        }
        # In case Lhaze becomes negative we reset it to zero to prevent artefacts.
        Lhaze [Lhaze < 0] <- 0
    }
    
    B_rescale	<- B_rescale[bandSet]
    G_rescale 	<- G_rescale[bandSet]
    esun <- esun[bandSet]
    
    if(satellite != "LANDSAT8"){
        
        if(!reflectance) {
            ## TOA Radiance
            xref <-  ( xref * G_rescale + B_rescale) / suntheta
        } else {
            ## At-surface reflectance (precalculate coefficients to speed up raster processing)
            C <- (pi * d ^ 2)/(TAUv * (esun * suntheta * TAUz + Edown))	
            b <- C * (B_rescale - Lhaze)
            a <- C * G_rescale 
            xref <-  a * xref  + b
        }
        
    } else {
        
        if(reflectance) {
            B_rescale 		<- metaData$UNIFIED_METADATA$REF_OFFSET[bandSet]
            G_rescale 		<- metaData$UNIFIED_METADATA$REF_GAIN[bandSet]
        } 
        
        ## At sensor radiance / reflectance
        xref <-  (G_rescale * xref + B_rescale) / suntheta
        
        ## At-surface reflectance?
    }
    
    ## Rename bands
    if(!reflectance){
        names(xref) <- gsub("DN", "TRD", names(xref))
    } else {
        if(method == "APREF") {
            names(xref) <- gsub("DN", "TRF", names(xref)) 
        } else {
            names(xref) <- gsub("DN", "SRF", names(xref))
        }
    }
    
      
        ## Re-combine thermal, solar and excluded imagery
        x <- stack(xref,xtir, xexc)
        
        bandOrder <- match(origBands, c(bandSet, tirBands, exclBands))
    x <- x[[bandOrder]]
    
    return(x)
}






