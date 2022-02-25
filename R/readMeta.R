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
        
        
        contGroup <- intersect( c("PRODUCT_METADATA", "PRODUCT_CONTENTS"), names(meta))
        if(!length(contGroup)) .raiseGroupNotFound("Content")
        
        projGroup <- intersect( c("PROJECTION_ATTRIBUTES", "PROJECTION_PARAMETERS"), names(meta))
        if(!length(projGroup)) .raiseGroupNotFound("Projection")
        
        radGroup <- intersect( c("RADIOMETRIC_RESCALING", "LEVEL1_RADIOMETRIC_RESCALING"), names(meta))
        
        
        rnlut <- melt(lapply(meta,rownames), value.name = "rn")
        .getParFromAnyGroup <- function(par,exg) {
            inc <- if (missing("exg")) rep(TRUE, nrow(rnlut)) else !grepl(exg, rnlut$L1) 
            m <- rnlut[grepl(par, rnlut$rn) & inc ,]
            if(nrow(m)){
                return(meta[[m$L1]][m$rn,]) 
            }
            return(NA)
        }
        
        ## Legacy MTL? 
        legacy <- "PROCESSING_SOFTWARE" %in% rownames(meta[[contGroup]])
        if(legacy) message("This scene was processed before August 29, 2012. Using MTL legacy format. Some minor infos such as SCENE_ID will be missing")
        
        sat     <- paste0("LANDSAT", .getNumeric( .getParFromAnyGroup("SPACECRAFT_ID")))
        sen     <- .getParFromAnyGroup("SENSOR_ID")               
        scene   <- .getParFromAnyGroup("LANDSAT_SCENE_ID") ## could assemble name for legacy files: http://landsat.usgs.gov/naming_conventions_scene_identifiers.php
        prodid  <- .getParFromAnyGroup("LANDSAT_PRODUCT_ID", "LEVEL1") 
        level   <- .getParFromAnyGroup("PROCESSING_LEVEL", "LEVEL1") 
        colNum  <- .getParFromAnyGroup("COLLECTION_NUMBER", "LEVEL1") 
        colTier <- .getParFromAnyGroup("COLLECTION_CATEGORY", "LEVEL1") 
        date    <- as.POSIXct(.getParFromAnyGroup("ACQUISITION_DATE|DATE_ACQUIRED"), tz="GMT")
        date    <- strptime(paste0(date, .getParFromAnyGroup("SCENE_CENTER_TIME")), "%Y-%m-%d %H:%M:%S", tz = "GMT")
        pdate   <- as.POSIXct(.getParFromAnyGroup("FILE_DATE|PRODUCT_CREATION_TIME|DATE_PRODUCT_GENERATED"), tz="GMT")
        path    <- as.numeric(.getParFromAnyGroup("^WRS_PATH"))
        row     <- as.numeric(.getParFromAnyGroup("^WRS_ROW|STARTING_ROW"))
        
        pars    <- meta[[projGroup]][c("MAP_PROJECTION","UTM_ZONE","DATUM", "ELLIPSOID"),]
        pars[1] <- tolower(pars[1])
        proj    <- CRS(paste0(c("+proj=", "+zone=", "+units=m +datum=", "+ellips="), pars, collapse=" "))
        
        rn <- row.names(meta[[contGroup]])
        
        geof <- meta[[contGroup]][grep("\\.TIF", meta[[contGroup]][,1]),,drop=F]
        files <- geof[,1]
        rn <- rownames(geof)
        bands <- regmatches(files, regexpr("(?<=[\\dQA]_)[^_]*(?=\\.TIF)|B6_VCID.*(?=\\.TIF)", files, perl = TRUE))
        
        qb <- grepl("QA|QUALITY",rn)
        ib <- grepl("B\\d",bands)
        ab <- grepl("ANGLE", rn)
        bands[ib] <- paste0(bands[ib],"_dn")
        bands[ab] <- paste0("AUX_", bands[ab])
        
        bands[qb] <- paste0("QA_", bands[qb])
        cat <- rep(NA,length(bands))
        cat[ib] <- "image"
        cat[ab] <- "aux"
        cat[qb] <- "qa"
        cat[grep("B8", bands)] <- "pan"
#        bands <- paste0(bands, "_dn")
        if(trailingZeros <- length(grep("0.TIF", bands)) > 1) stop("Trailing zeros")
        
        quant  <- rep("dn", length(bands))
        quant[ab] <- "angle"
        
        spatRes  <- rep(meta[[projGroup]]["GRID_CELL_SIZE_REFLECTIVE", ], length(bands))
        if(sen != "MSS") {
            spatRes[grep("B8", bands)] <- meta[[projGroup]]["GRID_CELL_SIZE_PANCHROMATIC", ]
            spatRes[grep("B6|B10|B11", bands)] <- meta[[projGroup]]["GRID_CELL_SIZE_THERMAL", ]
        }
        spatRes <-  as.numeric(spatRes)
        
        na      <- NA
        az      <- as.numeric(.getParFromAnyGroup("SUN_AZIMUTH"))
        selv    <- as.numeric(.getParFromAnyGroup("SUN_ELEVATION"))
        esd     <- as.numeric(.getParFromAnyGroup("EARTH_SUN_DISTANCE"))
        if(is.na(esd)) esd <- .ESdist(date)
        esd     <- as.numeric(esd)
        vsat    <- NA ## or set to max?
        scal    <- 1
        dtyp    <- NA
        
        ## Calculate ESUN the GRASS Way: https://grass.osgeo.org/grass64/manuals/i.landsat.toar.html
        # esun <- pi * esd * r[grepl("MAXIMUM", rownames(r)),,drop = F][1:9,]/  reflMax[grepl("MAXIMUM", rownames(reflMax)),,drop = F]
        
        
        ## RADIOMETRIC CORRECTION/RESCALING PARAMETERS     
        if(!legacy) {                 
            r <- meta[[radGroup]]
            rnr <- rownames(r)
            calrad    <- data.frame(offset = r[grep("RADIANCE_ADD*", rownames(r)),], gain = r[grep("RADIANCE_MULT*", rnr),])
            calref    <- data.frame(offset = r[grep("REFLECTANCE_ADD*", rownames(r)),], gain = r[grep("REFLECTANCE_MULT*", rnr),])
            rownames(calrad) <-  paste0(gsub("^.*BAND_","B", rnr[grep("RADIANCE_MULT", rnr)]), "_dn")
            if(nrow(calref) != 0)  rownames(calref) <-  paste0(gsub("^.*BAND_","B", rnr[grep("REFLECTANCE_MULT", rnr)]), "_dn") else calref <- NA
            
        } else {
            mimaGroup   <- intersect( c("MIN_MAX_RADIANCE", "LEVEL1_MIN_MAX_RADIANCE"), names(meta))
            mimapiGroup <- intersect( c("MIN_MAX_PIXEL_VALUE", "LEVEL1_MIN_MAX_PIXEL_VALUE"), names(meta))
            
            r      <- meta[[mimaGroup]]
            rp     <- meta[[mimapiGroup]]
            rnr    <- rownames(r)
            e2nd   <- seq(1, nrow(r), 2)
            L      <- diff(r[,1])[e2nd]                        
            Q      <- diff(rp[,1])[e2nd]                         
            radg   <- L/Q
            rado   <- r[seq(2,nrow(r),2),1] - radg                     
            calrad <- data.frame(offset = rado, gain = radg)
            calref <- NA
            rownames(calrad) <- paste0(gsub("^.*BAND_","B", rnr[grep("MAX", rnr)]), "_dn")                                  
        }
        
        if(sat == "LANDSAT8"){
            tirGroup <- intersect( c("TIRS_THERMAL_CONSTANTS", "LEVEL1_THERMAL_CONSTANTS"), names(meta))
            
            r <- meta[[tirGroup]]          
            calbt <- data.frame(K1 = r[grep("K1", rownames(r)), ],
                    K2  = r[grep("K2", rownames(r)), ])
            rownames(calbt) <- c("B10_dn", "B11_dn")
            
        } else {
            if(sen != "MSS") {
                TAB7 <- list(LANDSAT4 = data.frame(K1=671.62,K2=1284.3), # TAB7 from Chander 2009
                        LANDSAT5 = data.frame(K1=607.76,K2=1260.56),
                        LANDSAT7 = data.frame(K1=666.09,K2=1282.71))
                
                calbt <- TAB7[[sat]]            
                tbds  <- bands[grep("B6", bands)]
                if(length(tbds)>1) calbt <- rbind(calbt, calbt)
                rownames(calbt)<-tbds
            } else {
                calbt <- NULL
                tbds <- NULL
            }
        }
        
    } else {
        ## PROCESS ESPA LEDAPS XML FILES
        meta <- xmlToList(xmlParse(file))
        names(meta$bands) <- gsub(" ", "_", unlist(sapply(meta$bands, "[", "long_name")))
        
        if(raw) return(meta)
        
        luv     <- c(dn = "dn", toa_rad = "tra", toa_refl = "tre", toa_bt = "bt", sr_refl = "sre", spectral_indices = "idx", cfmask = "tre")
        atts    <- sapply(meta$bands, "[", ".attrs") 
        sat     <- paste0("LANDSAT", .getNumeric(meta$global_metadata$satellite))
        sen     <- meta$global_metadata$instrument
        scene   <- gsub("_MTL.txt", "", meta$global_metadata$lpgs_metadata_file)  ## could assemble name for legacy files: http://landsat.usgs.gov/naming_conventions_scene_identifiers.php
        date    <- strptime(paste(meta$global_metadata$acquisition_date,meta$global_metadata$scene_center_time), format = "%Y-%m-%d %H:%M:%OS", tz = "GMT")
        pdate   <- as.POSIXct(meta$bands[[1]]$production_date, tz = "GMT")
        path    <- as.numeric(meta$global_metadata$wrs["path"])
        row     <- as.numeric(meta$global_metadata$wrs["row"])
        az      <- as.numeric(meta$global_metadata$solar_angles["azimuth"])
        selv    <- 90 - as.numeric(meta$global_metadata$solar_angles["zenith"])        
        proj    <- CRS(paste0("+proj=utm +zone=",meta$global_metadata$projection_information$utm_proj_params," +datum=WGS84 +units=m"))
        esd     <- .ESdist(date)
        files   <- sapply(meta$bands, "[[", "file_name")   
        quant   <- luv[sapply(atts, "[", "product")]
        cat     <- sapply(atts, "[", "category") 
        cat[grep("opacity", names(cat))] <- "qa"
        bands    <- gsub(paste0(scene, "_|.tif"), "", files)                    
        bs         <- grepl("_surface_reflectance", names(files))
        bands[bs]  <- paste0("B", .getNumeric(bands[bs]), "_", quant[bs])
        bands[cat == "qa"] <- paste0("QA_", gsub("sr_|_qa", "", bands[cat == "qa"]))
        bands[cat == "index"] <- gsub("SR_", "", toupper(bands[cat == "index"]))
        spatRes <-  vapply(meta$bands,function(x) x$pixel_size["x"], character(1))
        na      <- as.numeric(sapply(atts, "[" , "fill_value"))
        vsat    <- as.numeric(sapply(atts, "[" , "saturate_value"))
        scal    <- as.numeric(sapply(atts, "[" , "scale_factor"))
        dataTypes <- c(INT16 = "INT4S", UINT8 = "INT1U")
        dtyp    <- dataTypes[as.character(sapply(atts, "[" , "data_type"))] 
        colTier <- NA ## to be fixed
        colNum  <- "01"  ## to be fixed
        ## Missing
        calrad <- calref <- calbt <- NA
        
    } 
    
    ## Add-on data:
    radRes    <- if(sat == "LANDSAT8") 16 else if(sen == "MSS") 6 else 8
    
    
    ImageMetaData(file = file, format = format, sat = sat, sen = sen, colNum = colNum, colTier = colTier, scene = scene, date = date, pdate = pdate, path = path, radRes=radRes, spatRes = spatRes, row = row, az = az,
            selv = selv, esd = esd, files = files, bands = bands, quant = quant, cat = cat, na = na, vsat = vsat, scal = scal, dtyp = dtyp, 
            calrad=calrad, calref=calref, calbt=calbt, proj = proj)
    
}   

.raiseGroupNotFound <- function(x) {
    stop(paste0(x," parameter group could not be identified.\nPlease file an issue at https://github.com/bleutner/RStoolbox/issues and include your MTL file."), call. = FALSE)
}

#' ImageMetaData Class
#' 
#' @param file Character. Metadata file
#' @param format Character. Metadata format, e.g. xml, mtl
#' @param sat Character. Satellite platform
#' @param sen Character. Sensor
#' @param scene Character. Scene_ID
#' @param colNum Character Collection number 
#' @param colTier Character Collection tier 
#' @param proj CRS. Projection.
#' @param date POSIXct. Aquisition date.
#' @param pdate POSIXct. Processing date.
#' @param path Integer. Path.
#' @param row Integer. Row.
#' @param az Numeric. Sun azimuth
#' @param selv Numeric. Sun elevation
#' @param esd Numeric. Earth-sun distance
#' @param files Character vector. Files containing the data, e.g. tiff files
#' @param bands Character vector. Band names
#' @param quant Character vector. Quantity, one of c("dn", "tra", "tre", "sre", "bt", "idx", "angle")
#' @param cat Character vector. Category, e.g. c("image", "pan", "index", "qa", "aux")
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
ImageMetaData <- function(file = NA, format = NA, sat = NA, sen = NA, scene = NA, colNum = NA, colTier = NA, 
        proj =NA, date = NA, pdate = NA,path = NA, row = NA, az = NA, selv = NA,
        esd = NA, files = NA, bands = NA, quant = NA, cat = NA, na = NA, vsat = NA, 
        scal = NA, dtyp = NA, calrad = NA, calref = NA, calbt = NA, radRes=NA, spatRes = NA){
    obj <- list(
            METADATA_FILE = file,
            METADATA_FORMAT = format,
            SATELLITE = sat,
            SENSOR = sen,
            SCENE_ID = scene,
            COLLECTION = c("collection" = colNum, "tier" = colTier),
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
    obj$DATA <- obj$DATA[with(obj$DATA, order(factor(CATEGORY, levels = c("image", "pan", "index", "qa", "aux")),
                            .getNumeric(BANDS),
                            factor(QUANTITY, levels = c("dn", "tra", "tre", "sre", "bt", "idx", "aux"))
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
