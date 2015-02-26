#' Retrieves Average Earth-Sun distance (in AU) for a given date 
#' 
#' based on 1970-2070 per-DOY average of NASA JPL HORIZONS calculations 
#' 
#' @param date character. date in format "YYYY-MM-DD" 
#' @keywords internal
#' @noRd 
.ESdist <- function(date){	
    doy <- as.numeric(format(as.POSIXct(date), "%j"))
    .ESdistance[doy]
}


#' Extract numbers from strings
#' 
#' @param x string or vector of strings
#' @param returnNumeric logical. should results be formatted \code{as.numeric}? If so, "05" will be converted to 5. Set returnNumeric to \code{FALSE} to keep preceeding zeros.
#' @note decimal numbers will be returned as two separate numbers
#' @keywords internal
#' @noRd 
.getNumeric <- function(x, returnNumeric = TRUE) {
    vapply(x, function(xi){
                d <- strsplit(xi, "[^[:digit:]]")[[1]]
                d <- if(returnNumeric) as.numeric(d[d!=""]) else d[d!=""]
                if(length(d)==0) d <- NA_real_
                d[1]
            }, numeric(1))
}

#' Run raster functions in parallel if possible
#' @param raster Raster* Object
#' @param rasterFun function. E.g. predict, calc, overlay 
#' @param args list. arguments to be passed to rasterFun.
#' @param wrArgs arguments to be passed to rasterFun, typically to writeRaster
#' @keywords internal
#' @noRd 
.paraRasterFun <- function(raster, rasterFun, args = list(), wrArgs = list()){
    if (isTRUE( getOption('rasterCluster'))) {
        do.call("clusterR", args = c(list(x = raster, fun = rasterFun, args=args), wrArgs))
    } else {
        do.call("rasterFun", args=c(raster, args, wrArgs))
    }
}

#' Run functions of ?apply family in parallel if possible
#' @param X
#' @param XFUN ?apply function. Currently c(sapply,lapply, apply)
#' @param MARGIN integer. Margin for apply.
#' @param FUN function to be ?applied
#' @param envir Environment in which to look for objects to export. Usually this should be environment()
#' @param ... further arguments passed to fun
#' @keywords internal
#' @examples
#' \dontrun{
#'  xList <- lapply(rep(1000,10000), rnorm)
#'  for(i in 1:2) {
#'     if(i == 2) raster::beginCluster(4, type="SOCK")
#'     RStoolbox:::.parXapply(xList, XFUN = "lapply", FUN = sum, na.rm = TRUE, envir = environment()),
#'     RStoolbox:::.parXapply(xList, XFUN = "sapply", FUN = sum, na.rm = TRUE, envir = environment()),
#'     RStoolbox:::.parXapply(matrix(100^2, 100,100), XFUN = "apply", MAR = 1, FUN = sum, na.rm = TRUE, envir = environment()),
#'     endCluster()
#'  }
#' }
#' @noRd 
.parXapply <- function(X, XFUN, MARGIN, FUN, envir, ...){   
    
    call <- quote(f(cl = cl, X = X, FUN = FUN, MARGIN = MARGIN, ...))
    
    if(isTRUE( getOption('rasterCluster'))) {
        cl <- getCluster()  
        on.exit(returnCluster()) 
        f  <- c(lapply=parLapply, sapply=parSapply, apply=parApply)[[XFUN]]
        if(!is.primitive(FUN)){
            g  <- findGlobals(FUN)
            gg <- lapply(g, get, envir = envir) 
            names(gg) <- g
        } else {
            gg<-NULL
        }
        l  <- c(list(...),gg)
        clusterExport(cl=cl, names(l), envir = envir)
        if(XFUN == "lapply") names(call)[names(call)=="FUN"] <- "fun"
    } else {
        f <- get(XFUN)
        call[["cl"]] <- NULL
    }    
    if(XFUN != "apply") call[["MARGIN"]] <- NULL
    eval(call)
    
}

#' Set-up doParallel backend when beginCluster has been called
#' 
#' this is to allow caret to run caret::train in parallel (via foreach) 
#' stopCluster will take place automatically on call to raster::endCluster
#' @keywords internal
#' @noRd 
.registerDoParallel <- function(){
    if(isTRUE(getOption('rasterCluster')) && !getDoParRegistered()) {
        cl <- raster::getCluster()
        registerDoParallel(cl)
    }
}

#' Get file extension for writeRaster
#' @param x character. Format driver.
#' @keywords internal
#' @noRd 
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
#' @noRd 
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
#' @noRd 
.df2tab <- function(x, align){
    c(paste0("\\tabular{", align, "}{"),
            paste(paste("\\strong{", colnames(x), "}", collapse = " \\tab "), "\\cr" ),
            paste(apply(x, 1, paste, collapse = " \\tab "), c(rep("\\cr", nrow(x)), "}")))
}

#' Convert character to numric band
#' @param raster Raster*
#' @param ... Character or Numeric bands
#' @keywords internal
#' @noRd 
.numBand <- function(raster, ...){
    bands <- list(...)
    lapply(bands, function(band) if(is.character(band)) which(names(raster) == band) else band ) 
}

.vMessage <- function(...){    
    if(getOption("RStoolbox.verbose")){message(...)}
}

#' On package startup
#' @noRd 
.onLoad <- function(libname, pkgname){
    options(RStoolbox.verbose = FALSE)
}

#' Clean up on package unload
#' @noRd 
.onUnload <- function (libpath) {
    library.dynam.unload("RStoolbox", libpath)
}
