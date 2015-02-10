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


#' Run raster functions in parallel if possible
#' @param raster Raster* Object
#' @param rasterFun function. E.g. predict, calc, overlay 
#' @param ... further arguments to be passed to rasterFun
#' @keywords internal
#' @examples 
.paraRasterFun <- function(raster, rasterFun, ...){
    args <- list(...)
    if (isTRUE(getOption("rasterCluster"))) {
        clusterR(x = raster, fun = rasterFun, args=args)
    } else {
        do.call("rasterFun", args =c(raster, args))
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
    ## TODO: add single dot / current directory treatment
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


#' Print data.frame in roxygen2 table format
#' @param x data.frame
#' @param align Character. Column aligntment in the form "llrc" 
#' @keywords internal
.df2tab <- function(x, align){
    c(paste0("\\tabular{", align, "}{"),
            paste(paste("\\strong{", colnames(x), "}", collapse = " \\tab "), "\\cr" ),
            paste(apply(x, 1, paste, collapse = " \\tab "), c(rep("\\cr", nrow(x)), "}")))
}

