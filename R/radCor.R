#' Radiometric calibration and correction
#' 
#' Implements several different methods for absolute radiometric correction of Landsat data.
#' You can either specify a metadata file, or supply all neccesary values manually. With proper parametrization APREF and SDOS should work for other sensors as well.
#' 
#' @param img raster object
#' @param metaData object of class ImageMetaData or a path to the meta data (MTL) file. 
#' @param radiance Logical. If \code{TRUE} only radiance will be calculated. Otherwise reflectance and brightness temperature are calculated. 
#' @param method Radiometric correction method to be used. There are currently four methods available (see Details):
#' "APREF", "SDOS", "DOS", "COSTZ".
#' @param bandSet numeric or character. original Landsat band numbers or names in the form of ("B1", "B2" etc). If set to 'full' all bands in the solar region will be processed.
#' @param SHV starting haze value, can be estimated using estimateSHV(). if not provided and method is "DOS" or "COSTZ" SHV will be estimated in an automated fashion. Not needed for apparent reflectance.
#' @param hazeBand band from which SHV was estimated.
#' @param atHaze character. Scene haze characteristics. Will be estimated if not expicilty provided. Must be one of \code{"veryClear", "clear", "moderate", "hazy"} or \code{"veryHazy"}.
#' @param darkProp numeric. Estimated proportion of dark pixels in the scene. Used only for automatic guessing of SHV.
#' @param verbose Logical. Print status information. 

#' @note This was originally a fork of randcorr in the landsat package. It may be slower, however it works on Raster* objects and hence is memory-safe.
#' @details 
#' 
#' Conversion to top of atmosphere radiance (\eqn{W/(m^2 * srad * \mum)}) 
#' Conversion to at-satellite brightness temperature (K)
#' Conversion to top of atmosphere reflectance (unitless) corrected for the sun angle
#' Estimation of at-surface spectral reflectance (unitless)
#'  
#' \describe{
#' \item{APREF}{Apparent reflectance}
#' \item{DOS}{Dark object subtratction following Chavez (1989)}
#' \item{COSTZ}{Dark object subtraction following Chaves(1996)}
#' \item{SDOS}{Simple dark object subtraction. Classical DOS, Lhaze must be estimated for each band separately.}
#' }
#'  
#' The implemented sun-earth distances neglect the earth's eccentricity. Instead it uses a 100 year daily average (1979-2070).
#' 
#' 
#' Atmospheric haze decay model according to Chavez (1989)
#' \describe{
#' \item{veryClear}{\eqn{\lambda^{-4.0}}}
#' \item{clear}{\eqn{\lambda^{-2.0}}}
#' \item{moderate}{\eqn{\lambda^{-1.0}}}
#' \item{hazy}{\eqn{\lambda^{-0.7}}}
#' \item{veryHazy}{\eqn{\lambda^{-0.5}}} 
#' }
#' @references S. Goslee (2011): Analyzing Remote Sensing Data in R: The landsat Package. Journal of Statistical Software 43(4).
#' @export
radCor <-	function(img, metaData, radiance = FALSE,  method = "APREF", bandSet = "full", SHV, hazeBand, atHaze, darkProp = 0.02, verbose){
    # http://landsat.usgs.gov/Landsat8_Using_Product.php
    
    if(!method %in% c("APREF", "DOS", "COSTZ", "SDOS")) stop("method must be one of 'APREF', 'DOS', 'COSTZ' 'SDOS'", call.=FALSE)
    
    if(!missing(verbose)) {verbold <- force(getOption("RStoolbox.verbose"))
        on.exit(options(RStoolbox.verbose = verbold))
        options(RStoolbox.verbose = verbose)
    }
    if(radiance & method != "APREF"){
        .vMessage("For radiance calculations the 'method' argument is ignored")
        method <- "APREF"
    }
    
    if(!grepl("MTL", metaData)) stop("The metadata file must be the original MTL file") 
    
    ## Read metadata from file
    if(is.character(metaData)) metaData <- readMeta(metaData)
    
    sat 		<- metaData$SATELLITE
    sensor 		<- metaData$SENSOR
    d			<- metaData$SOLAR_PARAMETERS["distance"]
    sunElev		<- metaData$SOLAR_PARAMETERS["elevation"]   
    rad 		<- metaData$DATA$RADIOMETRIC_RESOLUTION 
    
    if(sat == "LANDSAT8" & method != "APREF") {
        warning("DOS, COSTZ and SDOS are currently not implemented for Landsat 8. Using official reflectance calibration coefficients, i.e. output corresponds to method = 'APREF'", call. = FALSE) 
        method <- "APREF"
    }
    
    satZenith   <- 1
    satZenith	<- satZenith * pi / 180
    satphi 		<- cos(satZenith)
    suntheta 	<- cos((90 - sunElev) * pi / 180)	
    
    ## Query internal db	
    sDB <- .LANDSATdb[[sat]][[sensor]]
    
    if(any(bandSet == "full")) {
        bandSet <- names(img)
    } else {
        if(is.numeric(bandSet)) bandSet <- names(img)[bandSet]
    }	
    
    origBands 	<- names(img)   
    corBands 	<- sDB[!sDB$bandtype %in% c("TIR", "PAN"), "band"]
    bandSet 	<- bandSet[bandSet %in% corBands]
    
    tirBands	<- c("B10_dn", "B11_dn", "B6_dn", "B6_VCID_1_dn", "B6_VCID_2_dn")	
    tirBands 	<- origBands[origBands %in% tirBands]  
    if(length(tirBands) == 0) tirBands <- NULL
    
    exclBands	<- origBands[!origBands %in% c(bandSet, tirBands)]   
    excl 		<- if(length(exclBands) > 0) img[[exclBands]] else  NULL
    
    if(radiance) {
        bandSet <- c(bandSet, tirBands)
        .vMessage("Bands to convert to toa radiance: ", paste(bandSet, collapse = ", "))
    } else {
        .vMessage("Bands to convert to reflectance: ", paste(bandSet, collapse = ", "))
        if(length(tirBands) > 0) .vMessage("Thermal bands to convert to brightness temperatures: ", paste(tirBands, collapse=", "))
        if(length(exclBands) > 0) .vMessage("Excluding bands: ", paste(exclBands, collapse = ", "))	
    } 
    
    ## Thermal processing
    if(!radiance & length(tirBands) > 0) {
        .vMessage("Processing thermal band(s)")
        K1  	<- metaData$CALBT[tirBands, "K1"]
        K2   	<- metaData$CALBT[tirBands, "K2"]
        GAIN    <- metaData$CALRAD[tirBands,"gain"]
        OFFSET  <- metaData$CALRAD[tirBands,"offset"]           
        xtir <- .paraRasterFun(raster = img[[tirBands]], rasterFun = calc, args = list(fun = function(x) K2 / log(K1 / (GAIN * x + OFFSET) +1)))
        names(xtir) <- gsub("dn", "bt", tirBands)
    } else {
        xtir <- NULL
    }
    
    ## Radiance and reflectance processing
    GAIN    <- metaData$CALRAD[bandSet,"gain"]
    OFFSET  <- metaData$CALRAD[bandSet,"offset"]  
    TAUz <- 1
    TAUv <- 1
    Edown <- 0
    Lhaze <- 0   
    if(method != "APREF") {
        
        ## Estimate SHV automatically
        if(missing(SHV)){
            if(missing(hazeBand))  hazeBand <- names(img)[1]
            if(length(hazeBand) > 1) {
                stop("Automatic search for SHV values is intended for one band only. For more bands please estimate haze DNs manually using estimateSHV() \nhazeBand was automatically reset to 1")
                hazeBand <- names(img)[1]
            }
            .vMessage("SHV was not provided -> Estimating SHV automatically")
            ## We suppress warnings because we search for a possible value autimatically in case we missed the first time
            SHV <- suppressWarnings(estimateHaze(img, hazeBand = hazeBand, darkProp = darkProp , plot = FALSE, returnTables = TRUE))
            while(is.na(SHV[[1]])){ 
                darkProp	<- darkProp * 0.95
                SHV <- suppressWarnings(estimateHaze(SHV, hazeBand = hazeBand, darkProp = darkProp, plot = FALSE, returnTables = TRUE))
            }
            .vMessage(paste0("SHV estimated as: ", SHV[[1]]))
            SHV <- SHV[[1]]
        }
        
        
        if(method == "SDOS") {
            SHVdummy <- rep(0, length(bandSet))  
            names(SHVdummy) <- bandSet
            SHVdummy[hazeBand] <- SHV[hazeBand]
            SHV <- SHVdummy
            hazeBand <- bandSet
        }
        
        if (method == "COSTZ") {
            TAUz <- suntheta
            TAUv <- satphi
        }  
        
        ## 1% correction and conversion to radiance
        ## TODO: Calculate esun manually based on spectral response curves
        esun 	 <- sDB[hazeBand, "esun"]     
        GAIN_h 	 <- metaData$CALRAD[hazeBand,"gain"]
        OFFSET_h <- metaData$CALRAD[hazeBand,"offset"]
        Ldo  	 <- 0.01 * ((esun * suntheta * TAUz) + Edown) * TAUv / (pi * d ^ 2)
        Lhaze 	 <- (SHV * GAIN_h + OFFSET_h ) - Ldo
        
        if(method %in% c("DOS", "COSTZ")) {		
            ## Pick atmoshpere type
            if(missing(atHaze)) {
                atHaze.db <- data.frame(min = c(1,56,76,96,116), max = c(55,75,95,115,255)) / 255 * (2^rad-1)
                atHaze 	  <- c("veryClear", "clear", "moderate", "hazy", "veryHazy")[Lhaze > atHaze.db[,1] & Lhaze <= atHaze.db[,2]]
                .vMessage("Selecting atmosphere: '", atHaze, "'")
            }	
            if(is.numeric(hazeBand)) hazeBand <- names(img)[hazeBand]
            Lhaze	  <- Lhaze  * sDB[bandSet, paste0(hazeBand,"_", atHaze)] 
            ## Calculate corrected RAD_haze
            NORM  <- GAIN / GAIN_h
            Lhaze <- Lhaze * NORM + OFFSET
            
        }    
        
        # In case Lhaze becomes negative we reset it to zero to prevent artefacts.
        Lhaze [Lhaze < 0] <- 0
    }
    
    
    if(radiance)  { 
        ## Radiance
        layernames <- gsub("_dn", "_tra", bandSet)
    } else {
        ## Reflectance
        if(sat == "LANDSAT8"){
            GAIN 	<- metaData$CALRAD[bandSet,"gain"] / suntheta
            OFFSET 	<- metaData$CALREF[bandSet,"offset"] / suntheta          
        } else  {            
            esun 	<- sDB[bandSet, "esun"]            
            C		<- (pi * d ^ 2)/(TAUv * (esun * suntheta * TAUz + Edown))	
            OFFSET  <- C * (OFFSET - Lhaze)
            GAIN 	<- C * GAIN 
        }
        layernames <-   if(method == "APREF") gsub("_dn", "_tre", bandSet) else gsub("_dn", "_sre", bandSet)               
    }  
    .vMessage("Processing radiance / reflectance")  
    xref <- .paraRasterFun(img[[bandSet]], rasterFun = calc, args = list(fun = function(x) GAIN * x + OFFSET))
    names(xref) <- layernames   
    
    ## Re-combine thermal, solar and excluded imagery
    out <- stack(xref, xtir, excl)
    
    bandOrder <- match(origBands, c(bandSet, tirBands, exclBands))
    out <- out[[bandOrder]]
    
    return(out)
}






