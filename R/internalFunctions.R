#' Estimates Earth-Sun distance (in AU) for a given date 
#' 
#' Function taken from the landsat package: S. Goslee (2012)
#' 
#' @param adate character. date in format "YYYY-MM-DD" 
#' @keywords internal
.ESdist <- function(adate){	
    edist <- julian(as.Date(adate), origin=as.Date(paste(substring(adate, 1, 4), "12", "31", sep="-")))[[1]]
    1 - 0.016729 * cos((2*pi) * (0.9856 * (edist - 4)/360))
}


#' Extract numbers from strings
#' 
#' @param x string or vector of strings
#' @param returnNumeric logical. should results be formatted \code{as.numeric}? If so, "05" will be converted to 5. Set returnNumeric to \code{FALSE} to keep preceeding zeros.
#' @note decimal numbers will be returned as two separate numbers
#' @keywords internal
.getNumeric <- function(x, returnNumeric = TRUE) {
    sapply(x, function(xi){
                d <- strsplit(xi, "[^[:digit:]]")[[1]]
                d <- if(returnNumeric) as.numeric(d[d!=""]) else d[d!=""]
                d
            })
}


#' Run raster::predict in parallel if possible
#' @param raster Raster* Object
#' @param model model. E.g. randomForest model
#' @param na.rm logical.
#' @param further arguments to be passed to raster::predict
#' @keywords internal
#' @examples 
#' system.time(single <- paraPred(input, SC$model))
#' beginCluster(4, type="SOCK")
#' system.time(multi <- paraPred(input, SC$model))
#' endCluster()
#' all.equal(single, multi)
.paraPred <- function(object, model = model, na.rm = TRUE, ...){
    if (isTRUE(getOption("rasterCluster"))) {
        message("multicore")
        clusterR(x = object, fun = raster::predict, args=list(model = model, na.rm = na.rm, ...))
    } else {
        message("single core")
        raster::predict(object = object, model = model, na.rm = na.rm, ...)
    }
}


#' Get file extension for writeRaster
#' @param x character. Format driver.
#' @keywords internal
.rasterExtension <- function(x){ 
    fdb <- c(RASTER = ".grd", GTIFF = ".tif", CDF = ".nc", KML = ".kml", KMZ = ".kmz", BIG.MATRIX = ".big",
            BIL = ".bil", BSQ = ".bsq", BIP = ".bip",ASCII = ".asc", RST = ".rst",ILWIS = ".mpr", 
            SAGA = ".sdat",BMP = ".bmp", ADRG = ".gen", BT = ".bt", EHDR = ".bil",ENVI = ".envi",
            ERS = ".ers", GSBG = ".grd",HFA =  ".img", IDA =  ".img", RMF = ".rsw")
    ext <- fdb[toupper(x)]
    if(is.na(ext)) ".grd" else ext
}

#' Expand any filepath to the full path
#' @param x character. Path.
#' @keywords internal   
#' @examples 
#' .fullPath("test.grd")
#' .fullPath("../test.grd") 
#' .fullPath("../../../../../test.grd")
#' .fullPath("~/test.grd") 
#' .fullPath("/tmp/test.grd")
.fullPath <- function(x){
    x <- path.expand(x)   
    x <- gsub("\\\\", "/", x) ## anti-win
    x <- gsub("//", "/", x)   ## anti-win
    
    if(basename(x) == x | grepl("^[.][.]/", x))    x   <- file.path(getwd(),x)
    if(grepl("[.][.]", x)){
        xs  <- str_split(x, "/")[[1]]
        ups <- grep("[.][.]", xs)  
        rem <- c(ups, ups-length(ups))
        rem <- rem[rem > 1]
        xs  <-  xs[-rem]
        x   <- paste0(xs,collapse="/")      
    }
    x           
}   

