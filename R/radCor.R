#' Radiometric Calibration and Correction
#' 
#' Implements several different methods for radiometric calibraion and correction of Landsat data.
#' You can either specify a metadata file, or supply all neccesary values manually. 
#' With proper parametrization apref and sdos should work for other sensors as well.
#' 
#' @param img raster object
#' @param metaData object of class ImageMetaData or a path to the meta data (MTL) file. 
#' @param method Radiometric conversion/correction method to be used. There are currently four methods available (see Details):
#' "rad", "apref", "sdos", "dos", "costz".
#' @param bandSet Numeric or character. original Landsat band numbers or names in the form of ("B1", "B2" etc). If set to 'full' all bands in the solar (optical) region will be processed.
#' @param hazeValues Starting haze value, can be estimated using \link{estimateHaze}. if not provided and method is "dos" or "costz" hazeValues will be estimated in an automated fashion. Not needed for apparent reflectance.
#' @param hazeBands Bands corresponding to hazeValues.
#' @param atmosphere Character. Atmospheric characteristics. Will be estimated if not expicilty provided. Must be one of \code{"veryClear", "clear", "moderate", "hazy"} or \code{"veryHazy"}.
#' @param darkProp Numeric. Estimated proportion of dark pixels in the scene. Used only for automatic guessing of hazeValues (typically one would choose 1 or 2\%).
#' @param verbose Logical. Print status information. 
#' @note This was originally a fork of randcorr() function in the landsat package. This version works on Raster* objects and hence is suitable for large rasters.
#' @return 
#' RasterStack with top-of-atmosphere radiance (\eqn{W/(m^2 * srad * \mu m)}), at-satellite brightness temperature (K),
#' top-of-atmosphere reflectance (unitless) corrected for the sun angle or at-surface reflectance (unitless).
#' @details 
#' The atmospheric correction methods (sdos, dos and costz) apply to the optical (solar) region of the spectrum and do not affect the thermal band.
#' 
#' Dark object subtraction approaches rely on the estimation of atmospheric haze based on *dark* pixels. Dark pixels are assumed to have zero reflectance, hence the name.
#' It is then assumed further that any radiation originating from such *dark* pixels is due to atmospheric haze and 
#' not the reflectance of the surface itself.
#' 
#' The folloiwing \code{methods} are available:  
#' \tabular{ll}{
#' rad   \tab Radiance \cr
#' apref \tab Apparent reflectance (top-of-atmosphere reflectance) \cr
#' dos   \tab Dark object subtratction following Chavez (1989) \cr
#' costz \tab Dark object subtraction following Chavez (1996) \cr
#' sdos  \tab Simple dark object subtraction. Classical DOS, Lhaze must be estimated for each band separately. 
#' }
#' 
#' If either "dos" or "costz"  are selected, radCor will use the atmospheric haze decay model described by Chavez (1989).
#' Depending on the \code{atmosphere} the following coefficients are used:
#' \tabular{ll}{
#' veryClear \tab \eqn{\lambda^{-4.0}} \cr
#' clear     \tab \eqn{\lambda^{-2.0}} \cr
#' moderate  \tab \eqn{\lambda^{-1.0}} \cr
#' hazy      \tab \eqn{\lambda^{-0.7}} \cr
#' veryHazy  \tab \eqn{\lambda^{-0.5}} 
#' }
#' 
#' The implemented sun-earth distances neglect the earth's eccentricity. Instead we use a 100 year daily average (1979-2070).
#' @references S. Goslee (2011): Analyzing Remote Sensing Data in R: The landsat Package. Journal of Statistical Software 43(4).
#' @export
#' @examples 
#' library(raster)
#' ## Import meta-data and bands based on MTL file
#' mtlFile  <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")
#' metaData <- readMeta(mtlFile)
#' lsat     <- stackMeta(mtlFile)
#' 
#' \dontshow{
#' lsat <- readAll(lsat)
#' }
#' 
#' ## Convert DN to top of atmosphere reflectance and brightness temperature
#' lsat_ref <- radCor(lsat, metaData = metaData, method = "apref")
#' 
#' ## Correct DN to at-surface-reflecatance with DOS (Chavez decay model)
#' lsat_sref <- radCor(lsat, metaData = metaData, method = "dos")
#' 
#' ## Correct DN to at-surface-reflecatance with simple DOS 
#' ## Automatic haze estimation
#' hazeDN    <- estimateHaze(lsat, hazeBands = 1:4, darkProp = 0.01, plot = TRUE)
#' lsat_sref <- radCor(lsat, metaData = metaData, method = "sdos", 
#'                     hazeValues = hazeDN, hazeBands = 1:4)
radCor <-	function(img, metaData, method = "apref", bandSet = "full", hazeValues, hazeBands, atmosphere, darkProp = 0.02, verbose){
    # http://landsat.usgs.gov/Landsat8_Using_Product.php
    if(!missing("verbose")) .initVerbose(verbose)
    
    if(!method %in% c("rad", "apref", "dos", "costz", "sdos")) stop("method must be one of 'rad' 'apref', 'dos', 'costz' 'sdos'", call.=FALSE)
    
    
    ## Read metadata from file
    if(is.character(metaData)) {    
        if(!grepl("MTL", metaData)) stop("The metadata file must be the original MTL file") 
        metaData <- readMeta(metaData)
    } else if (!inherits(metaData, "ImageMetaData")){
        stop("metaData must be a path to the MTL file or an ImageMetaData object (see readMeta)")
    }
    sat 		<- metaData$SATELLITE
    sensor 		<- metaData$SENSOR
    d			<- metaData$SOLAR_PARAMETERS["distance"]
    sunElev		<- metaData$SOLAR_PARAMETERS["elevation"]   
    rad 		<- metaData$DATA$RADIOMETRIC_RESOLUTION 
    
    satZenith   <- 1
    satZenith	<- satZenith * pi / 180
    satphi 		<- cos(satZenith)
    suntheta 	<- cos((90 - sunElev) * pi / 180)	
    
    ## Query internal db	
    # TODO: add support for non-landsat data
    # The present implementation is geared towards use with Landsat 5:8 data. However, radCor can be used with other sensors as well (currently methods 'rad','apref','sdos' only). 
    # To do so create an \link{ImageMetaData} object containing the following information: 
    
    sDB <- .LANDSATdb[[sat]][[sensor]]
    
    if(any(bandSet == "full")) {
        bandSet <- names(img)
    } else {
        if(is.numeric(bandSet)) bandSet <- names(img)[bandSet]
    }	
    
    origBands 	<- names(img)   
    corBands 	<- sDB[!sDB$bandtype %in% c("TIR", "PAN"), "band"]
    bandSet 	<- bandSet[bandSet %in% corBands]
    
    tirBands	<- list(LANDSAT5 = "B6_dn", LANDSAT7 = c("B6_dn", "B6_VCID_1_dn", "B6_VCID_2_dn"), LANDSAT8 = c("B10_dn", "B11_dn") )[[sat]]	
    tirBands 	<- origBands[origBands %in% tirBands]  
    if(length(tirBands) == 0) tirBands <- NULL
    
    exclBands	<- origBands[!origBands %in% c(bandSet, tirBands)]   
    excl 		<- if(length(exclBands) > 0) img[[exclBands]] else  NULL
    
    if(method == "rad") {
        bandSet <- c(bandSet, tirBands)
        .vMessage("Bands to convert to toa radiance: ", paste(bandSet, collapse = ", "))
    } else {
        .vMessage("Bands to convert to reflectance: ", paste(bandSet, collapse = ", "))
        if(length(tirBands) > 0) .vMessage("Thermal bands to convert to brightness temperature: ", paste(tirBands, collapse=", "))
        if(length(exclBands) > 0) .vMessage("Excluding bands: ", paste(exclBands, collapse = ", "))	
    } 
    
    ## Thermal processing
    if((method != "rad") & (length(tirBands) > 0)) {
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
    if(!method %in% c("apref", "rad")) {
        
        ## Estimate hazeValues automatically
        if(missing(hazeValues)){
            if(missing(hazeBands))  hazeBands <- names(img)[1]
            if(length(hazeBands) > 1) {
                stop("Automatic search for hazeValues values is intended for one band only. For more bands please estimate haze DNs manually using estimatehazeValues() \nhazeBands was automatically reset to 1")
                hazeBands <- names(img)[1]
            }
            .vMessage("hazeValues was not provided -> Estimating hazeValues automatically")
            ## We suppress warnings because we search for a possible value autimatically in case we missed the first time
            hazeValues <- suppressWarnings(estimateHaze(img, hazeBands = hazeBands, darkProp = darkProp , plot = FALSE, returnTables = TRUE))
            while(is.na(hazeValues[[1]])){ 
                darkProp	<- darkProp * 0.95
                hazeValues <- suppressWarnings(estimateHaze(hazeValues, hazeBands = hazeBands, darkProp = darkProp, plot = FALSE, returnTables = TRUE))
            }
            .vMessage(paste0("hazeValues estimated as: ", hazeValues[[1]]))
            hazeValues <- hazeValues[[1]]
        }
        
        
        if(method == "sdos") {
            hazeValuesdummy <- rep(0, length(bandSet))  
            names(hazeValuesdummy) <- bandSet
            hazeValuesdummy[hazeBands] <- hazeValues[hazeBands]
            hazeValues <- hazeValuesdummy
            hazeBands <- bandSet
        }
        
        if (method == "costz") {
            TAUz <- suntheta
            TAUv <- satphi
        }  
        
        ## 1% correction and conversion to radiance
        ## TODO: Calculate esun manually based on spectral response curves
        esun 	 <- sDB[hazeBands, "esun"]     
        GAIN_h 	 <- metaData$CALRAD[hazeBands,"gain"]
        OFFSET_h <- metaData$CALRAD[hazeBands,"offset"]
        Ldo  	 <- 0.01 * ((esun * suntheta * TAUz) + Edown) * TAUv / (pi * d ^ 2)
        Lhaze 	 <- (hazeValues * GAIN_h + OFFSET_h ) - Ldo
        
        if(method %in% c("dos", "costz")) {		
            ## Pick atmoshpere type
            if(missing(atmosphere)) {
                atmosphere.db <- data.frame(min = c(1,56,76,96,116), max = c(55,75,95,115,255)) / 255 * (2^rad-1)
                atmosphere 	  <- c("veryClear", "clear", "moderate", "hazy", "veryHazy")[Lhaze > atmosphere.db[,1] & Lhaze <= atmosphere.db[,2]]
                .vMessage("Selecting atmosphere: '", atmosphere, "'")
            }	
            if(is.numeric(hazeBands)) hazeBands <- names(img)[hazeBands]
            Lhaze	  <- Lhaze  * sDB[bandSet, paste0(hazeBands,"_", atmosphere)] 
            ## Calculate corrected RAD_haze
            NORM  <- GAIN / GAIN_h
            Lhaze <- Lhaze * NORM + OFFSET
            
        }    
        
        # In case Lhaze becomes negative we reset it to zero to prevent artefacts.
        Lhaze [Lhaze < 0] <- 0
    }
    
    
    if(method == "rad")  { 
        ## Radiance
        layernames <- gsub("_dn", "_tra", bandSet)
    } else {
        ## Reflectance
        if(sat == "LANDSAT8"){
            GAIN 	<- metaData$CALREF[bandSet,"gain"] / suntheta
            OFFSET 	<- metaData$CALREF[bandSet,"offset"] / suntheta          
        } else  {            
            esun 	<- sDB[bandSet, "esun"]            
            C		<- (pi * d ^ 2)/(TAUv * (esun * suntheta * TAUz + Edown))	
            OFFSET  <- C * (OFFSET - Lhaze)
            GAIN 	<- C * GAIN 
        }
        layernames <-   if(method == "apref") gsub("_dn", "_tre", bandSet) else gsub("_dn", "_sre", bandSet)               
    }  
    .vMessage("Processing radiance / reflectance")  
    xref <- .paraRasterFun(img[[bandSet]], rasterFun = calc, args = list(fun = function(x) {gainOffsetRescale(x,GAIN,OFFSET)}, forcefun=TRUE))
    names(xref) <- layernames   
    
    ## Re-combine thermal, solar and excluded imagery
    out <- stack(xref, xtir, excl)
    
    bandOrder <- match(origBands, c(bandSet, tirBands, exclBands))
    out <- out[[bandOrder]]
    
    return(out)
}



