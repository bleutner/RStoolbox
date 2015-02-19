#' Read landsat MTL metadata files
#' 
#' Besides reading metadata, readMeta deals with legacy versions of Landsat metadata files and where possible adds missing information (radiometric gain and offset, earth-sun distance).
#' 
#' @param file path to Landsat MTL file (...MTL.txt)
#' @param raw Logical. If \code{TRUE} the full raw metadate will be returned as a list. All important metadata are homogenized into a standard format (ImgMetaData) and some information is added.
#' @return Object of class ImgMetaData 
#' @export 
#' 
readMeta <- function(file, raw = FALSE){
    if(!grepl("MTL", file) & !grepl("xml", file)) warning("The Landsat metadata file you have specified looks unusual. Typically the filename contains the string 'MTL' or 'xml'. Are you sure you specified the right file? \n I'll try to read it but check the results!")
    
    format <- if(grepl('xml', file)) "XML" else "MTL"
    
    if(format  == "MTL") {    
        ## PROCESS LPS MTL FILES
        meta <- read.delim(file, sep = "=", header = FALSE, stringsAsFactors = FALSE, strip.white = TRUE, skip = 1, skipNul = TRUE)
        meta <- meta[-(nrow(meta)-c(1,0)),]
        
        ## Retrieve groups
        l <- meta[grep("GROUP",meta[,1]),]
        
        ## Assemble metadata list
        meta <- lapply(unique(l[,2]), FUN = function(x){
                    w <- which(meta[,2] == x)
                    m <- meta[(w[1]+1):(w[2]-1),]
                    rownames(m) <- m[,1]
                    m <- m[ , 2, drop = FALSE]
                    colnames(m) <- "VALUE"
                    return(m)
                })   
        names(meta) <- unique(l[,2])
        
        num <- grep("IMAGE_AT|MIN_MAX|RESCAL|THERMAL", names(meta))
        meta[num] <- lapply(meta[num], function(x) {x[,1] <- as.numeric(x[,1]);x})
        
        if(raw) return(meta)
        
        ## Legacy MTL? 
        legacy <- "PROCESSING_SOFTWARE" %in% rownames(meta$PRODUCT_METADATA)
        if(legacy) message("This scene was processed before August 29, 2012. Using MTL legacy format. Some minor infos such as SCENE_ID will be missing")
        
        sat		<- paste0("LANDSAT", .getNumeric(meta$PRODUCT_METADATA["SPACECRAFT_ID",]))
        sen		<- meta$PRODUCT_METADATA["SENSOR_ID",]				
        scene	<- meta$METADATA_FILE_INFO["LANDSAT_SCENE_ID",]  ## could assemble name for legacy files: http://landsat.usgs.gov/naming_conventions_scene_identifiers.php
        date	<- as.POSIXct(if(!legacy) meta$PRODUCT_METADATA["DATE_ACQUIRED",] else meta$PRODUCT_METADATA["ACQUISITION_DATE",])
        date    <- strptime(paste0(date, meta$PRODUCT_METADATA["SCENE_CENTER_TIME",]), "%Y-%m-%d %H:%M:%S")
        pdate	<- as.POSIXct(if(!legacy) meta$METADATA_FILE_INFO["FILE_DATE",] else meta$METADATA_FILE_INFO["PRODUCT_CREATION_TIME",])
        path	<- as.numeric(meta$PRODUCT_METADATA["WRS_PATH",])
        row		<- if(!legacy) as.numeric(meta$PRODUCT_METADATA["WRS_ROW",]) else as.numeric(meta$PRODUCT_METADATA["STARTING_ROW",])
        
        pars <- meta$PROJECTION_PARAMETERS[c("MAP_PROJECTION","UTM_ZONE","DATUM"),]
        pars[1] <- tolower(pars[1])
        proj 	<-  CRS(paste0(c("+proj=", "+zone=", "+units=m +datum="), pars, collapse=" "))
        radRes	<- if(sat == "LANDSAT8") 16 else 8
        spatRes <- 30
        files	<- row.names(meta[["PRODUCT_METADATA"]])[grep("^.*BAND", row.names(meta$PRODUCT_METADATA))]
        files	<- meta[["PRODUCT_METADATA"]][files,]
        
        bands <- gsub(paste0(scene,"_|.TIF"), "", files)
        bands <- paste0(bands, "_dn")
        bands <- gsub("BQA", "QA", bands )
        
        if(trailingZeros <- length(grep("0.TIF", bands)) > 1) stop("Trailing zeros")
        
        prod <- rep("dn", length(bands))
        cat <-  rep("image", length(bands)) 
        cat[grep("QA", bands)] <- "qa"
        
        na		<- rep(NA, length(cat))
        az		<- if(!legacy) as.numeric(meta$IMAGE_ATTRIBUTES["SUN_AZIMUTH",]) else as.numeric(meta$PRODUCT_PARAMETERS["SUN_AZIMUTH",])
        selv	<- if(!legacy) as.numeric(meta$IMAGE_ATTRIBUTES["SUN_ELEVATION",]) else as.numeric(meta$PRODUCT_PARAMETERS["SUN_ELEVATION",])
        esd		<- meta$IMAGE_ATTRIBUTES["EARTH_SUN_DISTANCE",]
        if(is.null(esd) || is.na(esd)) esd <- .ESdist(date)
        esd		<- as.numeric(esd)
        vsat 	<- NA ## or set to max?
        scal 	<- 1
        dtyp 	<- NA
        ## RADIOMETRIC CORRECTION/RESCALING PARAMETERS     
        if(!legacy) { 	            
            r <- meta$RADIOMETRIC_RESCALING
            rnr <- rownames(r)
            calrad	<- data.frame(offset = r[grep("RADIANCE_ADD*", rownames(r)),], gain = r[grep("RADIANCE_MULT*", rownames(r)),])
            calref	<- data.frame(offset = r[grep("REFLECTANCE_ADD*", rownames(r)),], gain = r[grep("REFLECTANCE_MULT*", rownames(r)),])
            rownames(calrad) <-  paste0(gsub("^.*BAND_","B", rnr[grep("RADIANCE_MULT", rnr)]), "_dn")
            if(nrow(calref) != 0)  rownames(calref) <-  paste0(gsub("^.*BAND_","B", rnr[grep("REFLECTANCE_MULT", rnr)]), "_dn") else calref <- NA
            
        } else {
            
            r 		<- meta$MIN_MAX_RADIANCE
            rp		<- meta$MIN_MAX_PIXEL_VALUE
            rnr		<- rownames(r)
            e2nd 	<- seq(1, nrow(r), 2)
            L 		<- diff(r[,1])[e2nd]                        
            Q 		<- diff(rp[,1])[e2nd]                         
            radg	<- L/Q
            rado 	<- r[seq(2,nrow(r),2),1] - radg                     
            calrad 	<- data.frame(offset = rado, gain = radg)
            calref	<- NA
            rownames(calrad) <- paste0(gsub("^.*BAND_","B", rnr[grep("MAX", rnr)]), "_dn")                                  
        }
        
        if(sat == "LANDSAT8"){
            r <- meta$TIRS_THERMAL_CONSTANTS          
            calbt <- data.frame(K1 = r[1:2,] , K2 = r[3:4,])
            rownames(calbt) <- c("B10_dn", "B11_dn")
            
        } else {
            TAB7 <- list(LANDSAT4 = data.frame(K1=671.62,K2=1284.3), # TAB7 from Chander 2009
                    LANDSAT5 = data.frame(K1=607.76,K2=1260.56),
                    LANDSAT7 = data.frame(K1=666.09,K2=1282.71))
            
            calbt <- TAB7[[sat]]
            rownames(calbt)<-"B6_dn"
        }
        
    } else {
        ## PROCESS ESPA LEDAPS XML FILES
        meta <- xmlToList(xmlParse(file))
        names(meta$bands) <- str_replace_all(unlist(sapply(meta$bands, "[", "long_name")), " ", "_")
        
        if(raw) return(meta)
      
        luv		<- c(dn = "dn", toa_rad = "tra", toa_refl = "tre", toa_bt = "bt", sr_refl = "sre", spectral_indices = "idx", cfmask = "tre")
        atts 	<- sapply(meta$bands, "[", ".attrs") 
        sat		<- paste0("LANDSAT", .getNumeric(meta$global_metadata$satellite))
        sen 	<- meta$global_metadata$instrument
        scene 	<- str_replace(meta$global_metadata$lpgs_metadata_file, "_MTL.txt", "")  ## could assemble name for legacy files: http://landsat.usgs.gov/naming_conventions_scene_identifiers.php
        date	<- as.POSIXct(paste(meta$global_metadata$acquisition_date,meta$global_metadata$scene_center_time), "%Y%m%d %H:%M:%S" )
        pdate	<- as.POSIXct(meta$bands[[1]]$production_date)
        path	<- as.numeric(meta$global_metadata$wrs["path"])
        row		<- as.numeric(meta$global_metadata$wrs["row"])
        az		<- as.numeric(meta$global_metadata$solar_angles["azimuth"])
        selv	<- 90 - as.numeric(meta$global_metadata$solar_angles["zenith"])        
        proj <- CRS(paste0("+proj=utm +zone=",meta$global_metadata$projection_information$utm_proj_params," +datum=WGS84 +units=m"))
        esd		<- .ESdist(date)
        files	<- sapply(meta$bands, "[[", "file_name")   
        prod	<- luv[sapply(atts, "[", "product")]
        cat		<- sapply(atts, "[", "category") 
        cat[grep("opacity", names(cat))] <- "qa"
        
        bands	<- gsub(paste0(scene, "_|.tif"), "", files)					
        bs 	<- grepl("_band", files)
        bands[bs] 	<- paste0("B", .getNumeric(bands[bs]), "_", prod[bs])
        bands[cat == "qa"] <- paste0("QA_", gsub("sr_|_qa", "", bands[cat == "qa"]))
        bands[cat == "index"] <- gsub("SR_", "", toupper(bands[cat == "index"]))
        
        na		<- as.numeric(sapply(atts, "[" , "fill_value"))
        vsat	<- as.numeric(sapply(atts, "[" , "saturate_value"))
        scal	<- as.numeric(sapply(atts, "[" , "scale_factor"))
        dataTypes <- c(INT16 = "INT4S", UINT8 = "INT1U")
        dtyp	<- dataTypes[as.character(sapply(atts, "[" , "data_type"))]
        
        ## Missing
        calrad <- calref <- calbt <- NULL
        
    } 
    ImgMetaData(file = file, format = format, sat = sat, sen = sen, scene = scene, date = date, pdate = pdate, path = path, row = row, az = az,
            selv = selv, esd = esd, files = files, bands = bands, prod = prod, cat = cat, na = na, vsat = vsat, scal = scal, dtyp = dtyp, 
            calrad=calrad, calref=calref, calbt=calbt, proj = proj)
    
}   


## TODO: document ImgMetaData
ImgMetaData <- function(file = NA, format = NA, sat = NA, sen = NA,scene = NA, proj =NA, date = NA, pdate = NA,path = NA, row = NA, az = NA, selv = NA,
        esd = NA, files = NA, bands = NA, prod = NA, cat = NA, na = NA, vsat = NA, scal = NA, dtyp = NA, calrad = NA, calref = NA, calbt = NA){
    obj <- list(
            METADATA_FILE = file,
            METADATA_FORMAT = format,
            SATELLITE = sat,
            SENSOR = sen,
            SCENE_ID = scene,
            ACQUISITION_DATE = date,
            PROCESSING_DATE = pdate,
            PATH = path,
            ROW = row, 
            PROJECTION = proj,
            SUN_AZIMUTH = az,
            SUN_ELEVATION = selv,
            EARTH_SUN_DISTANCE = esd,
            DATA = data.frame(
                    FILES = files,
                    BANDS = bands,
                    PRODUCT = prod,                         
                    CATEGORY = cat,
                    NA_VALUE =  na,
                    SATURATE_VALUE =  vsat,
                    SCALE_FACTOR =  scal,
                    DATA_TYPE =  dtyp
            ),
            CALRAD = calrad,
            CALREF = calref,
            CALBT = calbt
    
    )
    
    ## Re-order DATA
    obj$DATA <- obj$DATA[with(obj$DATA, order(factor(CATEGORY, levels = c("image", "index", "qa")),
                            factor(PRODUCT, levels = c("dn", "tra", "tre", "sre", "bt", "idx")),
                            BANDS)),]
    
    structure(obj, class = c("ImageMetaData", "RStoolbox"))
    
}



#' Import separate Landsat files into single stack
#' 
#' Reads Landsat MTL or XML metadata files and loads single Landsat Tiffs into a rasterStack.
#' Be aware that by default stackLS() does NOT import panchromatic bands nor thermal bands with resolutions != 30m.
#' 
#' @param file character. Path to Landsat MTL metadata file (not an XML file!).
#' @param allResolutions logical. if \code{TRUE} a list will be returned with length = unique spatial resolutions.
#' @param type character vector. Which type of data should be returned in the stack? (only relevant for LS8 and LEDAPS processed products). 'image': image data, 'index': multiband indices, 'qa' quality flag bands.
#' @param product Character vector. Which products should be returned. Options: digital numbers ('dn'), top of atmosphere reflectance ('tre'), surface reflectance ('sre'). Only relevant for xml metadata from ESPA.
#' @return Either a list of rasterStacks comprising all resolutions or only one rasterStack comprising only 30m resolution imagery
#' @note 
#' Be aware that by default stackLS() does NOT import panchromatic bands nor thermal bands with resolutions != 30m. Use the allResolutions argument to import all layers.
#' 
#' The USGS uses cubic convolution to resample TIR bands to 30m resolution. In the opinion of the author this may not be the best choice for supersampling. 
#' Therefore the default method in this implementation is nearest neighbor. Keep this in mind if you plan to compare TIR bands created by differing resampling routines.
#' Typically, however, you will already have the USGS 30m TIR products, so no need to worry...
#' @export 
stackMeta <- function(file, allResolutions = FALSE, product = "all", type = "image"){
    ## TODO: check arguments
    stopifnot( !any(!type %in%  c("image", "index", "qa", "all")), !any(!product %in% c("all", "dn", "tra", "tre", "sre", "bt", "idx")))
   
    ## Read metadata and extract layer file names
    meta  <- readMeta(file)
    files <- meta$DATA$FILES   
    
    if("all" %in% product) product <- unique(meta$DATA$PRODUCT)
    if("all" %in% type) type <- unique(meta$DATA$CATEGORY)
    proAvail <- product %in% meta$DATA$PRODUCT
    typAvail <- type %in% meta$DATA$CATEGORY
    if(sum(proAvail)  == 0) stop("None of the specifed products exist according to the metadata. You specified:", paste0(product, collapse=", "), call.=FALSE )
    if(any(!proAvail)) warning("The following specified products don't exist: ", paste0(product[!proAvail], collapse=", ") ,"\nReturning available products:", paste0(product[proAvail], collapse=", "), call.=FALSE)
    if(sum(typAvail)  == 0) stop("None of the specifed types exists according to the metadata. You specified:", paste0(type, collapse=", "), call.=FALSE )
    if(any(!typAvail)) warning("The following specified types don't exist: ", paste0(type[!typAvail], collapse=", ") ,"\nReturning available types:", paste0(type[typAvail], collapse=", "), call.=FALSE)
    
    ## Load layers
    path  <- if(basename(file) != file)  gsub(basename(file), "", file) else NULL
    
    ## Import rasters
    rl <- lapply(paste0(path, files), raster)
    resL <- lapply(lapply(rl, res),"[", 1)
    
    if(any(resL > 30))     message("Your Landsat data includes TIR band(s) which were not resampled to 30m.")
    
    ## Stack
    returnRes <- if(allResolutions) unlist(unique(resL)) else 30
    
    ## Select products to return
    select <- meta$DATA$BANDS[
            meta$DATA$CATEGORY %in% type &
                    meta$DATA$PRODUCT %in% product
    ]               
 
    LS 	<- lapply(returnRes, function(x){
                s			<- stack(rl[resL == x])
                names(s) 	<- meta$DATA$BANDS[resL == x]
                s[[ which(names(s) %in% select)]]
            })
    
    if(!allResolutions) LS <- LS[[1]]
    
    return(LS)
}
