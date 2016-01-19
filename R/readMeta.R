#' Read Landsat MTL metadata files
#' 
#' Reads metadata and deals with legacy versions of Landsat metadata files and where possible adds missing information (radiometric gain and offset, earth-sun distance).
#' 
#' @param file path to Landsat MTL file (...MTL.txt)
#' @param raw Logical. If \code{TRUE} the full raw metadata will be returned as a list. if \code{FALSE} (the default) all important metadata are homogenized into a standard format (ImageMetaData) and some information is added.
#' @return Object of class ImageMetaData 
#' @export 
#' @examples 
#' ## Example metadata file (MTL)
#' mtlFile  <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")
#' 
#' ## Read metadata
#' metaData <- readMeta(mtlFile)
#' 
#' ## Summary
#' summary(metaData)
#' 
readMeta <- function(file, raw = FALSE){
    ## TODO: make modular for additional sensors
    if(!file.exists(file)) stop("Metadata file does not exist. Looking for: ", file, call. = FALSE)
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
        
        ## Numeric slots
        num <- grep("MIN_MAX|RESCAL|THERMAL", names(meta))      
        meta[num] <- lapply(meta[num], function(x) {
                    x[,1] <- as.numeric(x[,1])
                    x
                })
        
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
        proj 	<- CRS(paste0(c("+proj=", "+zone=", "+units=m +datum="), pars, collapse=" "))
        files	<- row.names(meta[["PRODUCT_METADATA"]])[grep("^.*BAND", row.names(meta$PRODUCT_METADATA))]
        files	<- meta[["PRODUCT_METADATA"]][files,]
        
        bands <- gsub(paste0(scene,"_|.TIF"), "", files)
        bands <- paste0(bands, "_dn")
        bands <- gsub("BQA", "QA", bands )
        
        if(trailingZeros <- length(grep("0.TIF", bands)) > 1) stop("Trailing zeros")
        
        quant  <- rep("dn", length(bands))
        cat	  <-  rep("image", length(bands)) 
        cat[grep("QA", bands)] <- "qa"
        cat[grep("B8", bands)] <- "pan"
        spatRes  <- rep(meta$PROJECTION_PARAMETERS["GRID_CELL_SIZE_REFLECTIVE", ], length(bands))
        spatRes[grep("B8", bands)] <- meta$PROJECTION_PARAMETERS["GRID_CELL_SIZE_PANCHROMATIC", ]
        spatRes[grep("B6|B10|B11", bands)] <- meta$PROJECTION_PARAMETERS["GRID_CELL_SIZE_THERMAL", ]
        spatRes <-  as.numeric(spatRes)
        
        na		<- NA
        az		<- if(!legacy) as.numeric(meta$IMAGE_ATTRIBUTES["SUN_AZIMUTH",]) else as.numeric(meta$PRODUCT_PARAMETERS["SUN_AZIMUTH",])
        selv	<- if(!legacy) as.numeric(meta$IMAGE_ATTRIBUTES["SUN_ELEVATION",]) else as.numeric(meta$PRODUCT_PARAMETERS["SUN_ELEVATION",])
        esd		<- meta$IMAGE_ATTRIBUTES["EARTH_SUN_DISTANCE",]
        if(is.null(esd) || is.na(esd)) esd <- .ESdist(date)
        esd		<- as.numeric(esd)
        vsat 	<- NA ## or set to max?
        scal 	<- 1
        dtyp 	<- NA
        
       ## Calculate ESUN the GRASS Way: https://grass.osgeo.org/grass64/manuals/i.landsat.toar.html
      # esun <- pi * esd * r[grepl("MAXIMUM", rownames(r)),,drop = F][1:9,]/  reflMax[grepl("MAXIMUM", rownames(reflMax)),,drop = F]
       
        
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
            tbds  <- bands[grep("B6", bands)]
            if(length(tbds)>1) calbt <- rbind(calbt, calbt)
            rownames(calbt)<-tbds
        }
        
    } else {
        ## PROCESS ESPA LEDAPS XML FILES
        meta <- xmlToList(xmlParse(file))
        names(meta$bands) <- gsub(" ", "_", unlist(sapply(meta$bands, "[", "long_name")))
        
        if(raw) return(meta)
        
        luv		<- c(dn = "dn", toa_rad = "tra", toa_refl = "tre", toa_bt = "bt", sr_refl = "sre", spectral_indices = "idx", cfmask = "tre")
        atts 	<- sapply(meta$bands, "[", ".attrs") 
        sat		<- paste0("LANDSAT", .getNumeric(meta$global_metadata$satellite))
        sen 	<- meta$global_metadata$instrument
        scene 	<- gsub("_MTL.txt", "", meta$global_metadata$lpgs_metadata_file)  ## could assemble name for legacy files: http://landsat.usgs.gov/naming_conventions_scene_identifiers.php
        date	<- as.POSIXct(paste(meta$global_metadata$acquisition_date,meta$global_metadata$scene_center_time), "%Y%m%d %H:%M:%S" )
        pdate	<- as.POSIXct(meta$bands[[1]]$production_date)
        path	<- as.numeric(meta$global_metadata$wrs["path"])
        row		<- as.numeric(meta$global_metadata$wrs["row"])
        az		<- as.numeric(meta$global_metadata$solar_angles["azimuth"])
        selv	<- 90 - as.numeric(meta$global_metadata$solar_angles["zenith"])        
        proj 	<- CRS(paste0("+proj=utm +zone=",meta$global_metadata$projection_information$utm_proj_params," +datum=WGS84 +units=m"))
        esd		<- .ESdist(date)
        files	<- sapply(meta$bands, "[[", "file_name")   
        quant	<- luv[sapply(atts, "[", "product")]
        cat		<- sapply(atts, "[", "category") 
        cat[grep("opacity", names(cat))] <- "qa"
        
        bands	<- gsub(paste0(scene, "_|.tif"), "", files)					
        bs 		<- grepl("_band", files)
        bands[bs] 	<- paste0("B", .getNumeric(bands[bs]), "_", quant[bs])
        bands[cat == "qa"] <- paste0("QA_", gsub("sr_|_qa", "", bands[cat == "qa"]))
        bands[cat == "index"] <- gsub("SR_", "", toupper(bands[cat == "index"]))
        spatRes <-  vapply(meta$bands,function(x) x$pixel_size["x"], character(1))
        na		<- as.numeric(sapply(atts, "[" , "fill_value"))
        vsat	<- as.numeric(sapply(atts, "[" , "saturate_value"))
        scal	<- as.numeric(sapply(atts, "[" , "scale_factor"))
        dataTypes <- c(INT16 = "INT4S", UINT8 = "INT1U")
        dtyp	<- dataTypes[as.character(sapply(atts, "[" , "data_type"))] 
        
        ## Missing
        calrad <- calref <- calbt <- NA
        
    } 
    
    ## Add-on data:
    radRes	<- if(sat == "LANDSAT8") 16 else 8
    
    
    ImageMetaData(file = file, format = format, sat = sat, sen = sen, scene = scene, date = date, pdate = pdate, path = path, radRes=radRes, spatRes = spatRes, row = row, az = az,
            selv = selv, esd = esd, files = files, bands = bands, quant = quant, cat = cat, na = na, vsat = vsat, scal = scal, dtyp = dtyp, 
            calrad=calrad, calref=calref, calbt=calbt, proj = proj)
    
}   


#' ImageMetaData Class
#' 
#' @param file Character. Metadata file
#' @param format Character. Metadata format, e.g. xml, mtl
#' @param sat Character. Satellite platform
#' @param sen Character. Sensor
#' @param scene Character. Scene_ID
#' @param proj CRS. Projection.
#' @param date POSIXct. Aquosition date.
#' @param pdate POSIXct. Processing date.
#' @param path Integer. Path.
#' @param row Integer. Row.
#' @param az Numeric. Sun azimuth
#' @param selv Numeric. Sun elevation
#' @param esd Numeric. Earth-sun distance
#' @param files Character vector. Files containing the data, e.g. tiff files
#' @param bands Character vector. Band names
#' @param quant Character vector. Quantity, one of c("dn", "tra", "tre", "sre", "bt", "idx")
#' @param cat Character vector. Category, e.g. c("image", "pan", "index", "qa")
#' @param na Numeric vector. No-data value per band
#' @param vsat Numeric vector. Saturation value per band
#' @param scal Numeric vector. Scale factor per band. e.g. if data was scaled to 1000*reflectance for integer conversion.
#' @param dtyp Character vector. Data type per band. See \code{\link[raster]{dataType}} for options.
#' @param radRes Numeric vector. Radiometric resolution per band.
#' @param spatRes Numeric vector. Spatial resolution per band.
#' @param calrad data.frame. Calibration coefficients for dn->radiance conversion. Must have columns 'gain' and 'offset'. Rows named according to \code{bands}.
#' @param calref data.frame. Calibration coefficients for dn->reflectance conversion. Must have columns 'gain' and 'offset'. Rows named according to \code{bands}.
#' @param calbt data.frame. Calibration coefficients for dn->brightness temperature conversion. Must have columns 'K1' and 'K2'. Rows named according to \code{bands}.
#' @export
ImageMetaData <- function(file = NA, format = NA, sat = NA, sen = NA, scene = NA, proj =NA, date = NA, pdate = NA,path = NA, row = NA, az = NA, selv = NA,
        esd = NA, files = NA, bands = NA, quant = NA, cat = NA, na = NA, vsat = NA, scal = NA, dtyp = NA, calrad = NA, calref = NA, calbt = NA, radRes=NA, spatRes = NA){
    obj <- list(
            METADATA_FILE = file,
            METADATA_FORMAT = format,
            SATELLITE = sat,
            SENSOR = sen,
            SCENE_ID = scene,
            ACQUISITION_DATE = date,
            PROCESSING_DATE = pdate,
            PATH_ROW = c(path=path,row=row), 
            PROJECTION = proj,
            SOLAR_PARAMETERS = c(azimuth=az, elevation = selv, distance = esd),
            DATA = data.frame(
                    FILES = files,
                    BANDS = bands,
                    QUANTITY = quant,                         
                    CATEGORY = cat,
                    NA_VALUE =  na,
                    SATURATE_VALUE =  vsat,
                    SCALE_FACTOR =  scal,
                    DATA_TYPE =  dtyp,
                    SPATIAL_RESOLUTION = spatRes,
                    RADIOMETRIC_RESOLUTION = radRes,
                    stringsAsFactors=FALSE
            ),
            CALRAD = calrad,
            CALREF = calref,
            CALBT = calbt
    
    )
    if(length(bands) == 1 && is.na(bands)) BANDS <- bands <- "1"
    rownames(obj$DATA) <- bands
    
    ## Re-order DATA
    obj$DATA <- obj$DATA[with(obj$DATA, order(factor(CATEGORY, levels = c("image", "pan", "index", "qa")),
                            .getNumeric(BANDS),
                            factor(QUANTITY, levels = c("dn", "tra", "tre", "sre", "bt", "idx"))
                    )),]
    
    structure(obj, class = c("ImageMetaData", "RStoolbox"))    
}


#' @method summary ImageMetaData
#' @export  
summary.ImageMetaData <- function(object, ...) { 
    
    labs <- format(c("Scene:", "Satellite:", "Sensor:", "Date:", "Path/Row:", "Projection:")) 
    vals <- c(object$SCENE_ID, object$SATELLITE,object$SENSOR,format(object$ACQUISITION_DATE, "%F"), paste(object$PATH_ROW, collapse="/"), projection(object$PROJECTION))
    cat(paste(labs, vals), fill =1)
    
    cat("\nData:\n") 
    print(object$DATA[, c("FILES", "QUANTITY", "CATEGORY")])    
    hasCal <- vapply(object[c("CALRAD", "CALREF", "CALBT")], is.data.frame, logical(1))
    
    cat("\nAvailable calibration parameters (gain and offset):\n") 
    if(any(hasCal)) {
        cat(c("\tdn -> radiance (toa)", "\tdn -> reflectance (toa)", "\tdn -> brightness temperature (toa)")[hasCal], sep = "\n")
    }   else {
        cat("\tnone")
    }
    cat("\n")
    invisible(NULL)
}
