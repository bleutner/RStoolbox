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

#' Convert to terra::SpatRaster
#'
#' @param x raster or terra object
#' @return RasterStack
#' @keywords internal
#' @noRd
.toTerra <- function(x) {
  if (inherits(x, "Raster")) {
    return(terra::rast(x))
  } else if (inherits(x, "Extent")) {
    return(terra::ext(x))
  } else {
    return(x)
  }
}

#' Convert sf objects
#'
#' @param x spatial sp object to sf
#' @return sf object
#' @keywords internal
#' @noRd
.toSf <- function(x) {
    if (inherits(x, "Spatial")) {
        return(st_as_sf(x))
    } else {
        return(x)
    }
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

#' Run raster functions
#' @param raster SpatRaster
#' @param rasterFun function. E.g. predict, calc, overlay 
#' @param args list. arguments to be passed to rasterFun.
#' @param wrArgs arguments to be passed to rasterFun, typically to writeRaster
#' @keywords internal
#' @noRd 
.paraRasterFun <- function(raster, rasterFun, args = list(), wrArgs = list()){
  do.call(rasterFun, c(list(raster), args, wrArgs))
}

#' Run functions of ?apply family
#' @param X object
#' @param XFUN ?apply function. Currently c(sapply,lapply, apply)
#' @param MARGIN integer. Margin for apply.
#' @param FUN function to be ?applied
#' @param envir Environment in which to look for objects to export. Usually this should be environment()
#' @param ... further arguments passed to fun
#' @keywords internal
#' @noRd
#' @examples
#' xList <- lapply(rep(1000,10000), rnorm)
#' RStoolbox:::.parXapply(xList, XFUN = "lapply", FUN = sum, na.rm = TRUE, envir = environment())
#' RStoolbox:::.parXapply(xList, XFUN = "sapply", FUN = sum, na.rm = TRUE, envir = environment())
#' RStoolbox:::.parXapply(matrix(100^2, 100,100), XFUN = "apply", MAR = 1, FUN = sum, na.rm = TRUE, envir = environment())
#'
.parXapply <- function(X, XFUN, MARGIN, FUN, envir, ...){
    call <- quote(f(cl = cl, X = X, FUN = FUN, MARGIN = MARGIN, ...))
    f <- get(XFUN)
    call[["cl"]] <- NULL
    if(XFUN != "apply") call[["MARGIN"]] <- NULL
    eval(call)

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
    
    if(basename(x) == x | grepl("^[.][.]/", x)){
      x   <- file.path(getwd(),x)
    }

    if(grepl("[.][.]", x)){
        xs  <- strsplit(x, "/")[[1]]
        ups <- grep("[.][.]", xs)  
        rem <- c(ups, ups-length(ups))
        rem <- rem[rem > 1]
        xs  <-  xs[-rem]
        x   <- paste0(xs,collapse="/")      
    }
    x           
}   

#' Update layer names after calc/overlay/predict etc.
#' this is useful if a filename was specified during calculations but 
#' layernames can only be changed in retrospect. If we don't do this and a file
#' was written to disk (filename), it would not have the new names.
#' @param x Raster
#' @param n Character. New names
#' @keywords internal
#' @noRd 
.updateLayerNames<-function(x, n){
    if(!identical(names(x),n)){
        names(x) <- n
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
            paste(apply(x, 1, paste, collapse = " \\tab "), c(rep("\\cr", nrow(x)-1), "}")))
}

#' Convert character to numric band
#' @param SpatRaster raster
#' @param ... Character or Numeric bands
#' @keywords internal
#' @noRd 
.numBand <- function(raster, ...){
    bands <- list(...)
    lapply(bands, function(band) if(is.character(band)) which(names(raster) == band) else band ) 
}

#' Print message if global RStoolbox.verbose option is TRUE
#' @keywords internal
#' @noRd 
.vMessage <- function(...){    
    if(getOption("RStoolbox.verbose")){message(format(Sys.time(), "%H:%M:%S | "), ...)}
}


#' Get overlap extent from multiple Extent objects (set union)
#' 
#' Can be achieved by raster::intersect as well, however .getExtentOverlap()
#' can deal with more than two extents.
#' 
#' @param ... Extent objects to combine
#' @return Extent object
#' @noRd
.getExtentOverlap <- function(...){
    el <- list(...)
    if(any(!vapply(el, inherits, what = "SpatExtent", logical(1)))) stop("You can only supply Extent objects to getExtentOverlap")
    em <- do.call("rbind", lapply(el, as.vector))
    ext(c(max(em[,1]), min(em[,2]), max(em[,3]), min(em[,4])))
}

#' Get center coordinates of Extent object or any object from which an extent can be derived
#' @param x Spatial object from which an extent can be derived
#' @return Vector of length two with center coordinate
#' @noRd 
.extentCenter <- function(x){
    c(xmax(x) + xmin(x), ymax(x) + ymin(x))/2 
}



#' Check haveminmax slot
#' @param x SpatRaster
#' @noRd 
#' @keywords internal
.hasMinMax <- function(x) {
    if(inherits(x, "SpatRaster")) {
        print(x)
        return(x@data@haveminmax)
    } else {
        return(vapply(1:nlyr(x), function(xi) {x[[xi]]@data@haveminmax}, logical(1)))
    }
}

##' Subdivide polygons into smaller polygons
##' @param polygons SpatialPolygonsDataFrame
##' @param res Numeric. Spatial resolution of subdivition grid
##' @noRd 
##' @keywords internal
#.subdividePolys <- function(polygons, res = 1) {
#    pl <- lapply(seq_along(polygons), function(i){
#                ex      <- raster(polygons[i,])
#                res(ex) <- res
#                pgrid   <- rasterToPolygons(ex)
#                pgrid$layer <- 1
#                pp    <- gIntersection(pgrid, polygons[i,], byid=TRUE, drop_lower_td = TRUE)
#                pp    <- as(pp, "SpatialPolygonsDataFrame")
#                data  <- polygons@data[i,]
#                pp@data <- data.frame(data, rn = paste0(rownames(data),"_", seq_along(pp)), row.names = "rn")
#                pp <- spChFIDs(pp, rownames(pp@data))
#                pp
#            })
#    plo <- do.call("rbind", pl)
#    projection(plo) <- projection(polygons)
#    return(plo)
#}


#' RMSE
#' @param pred predicted values
#' @param obs observed values
#' @noRd 
#' @keywords internal
.rmse <- function (pred, obs) {
    sqrt(mean((pred - obs)^2, na.rm = T))
}



#' On package startup
#' @noRd 
.onLoad <- function(libname, pkgname){
    if(is.null(getOption("RStoolbox.verbose")))  options(RStoolbox.verbose = FALSE)
}

#' Init verbosity within functions 
#' 
#' will restore global options after function has been called
#' @param verbose Logical
#' @keywords internal
#' @noRd 
.initVerbose <- function(verbose){
    verbold <- force(getOption("RStoolbox.verbose"))
    do.call("on.exit", list(substitute(options(RStoolbox.verbose = verbold))), envir=parent.frame())
    options(RStoolbox.verbose = verbose)
}

 
 
#' Clean up on package unload
#' @noRd 
.onUnload <- function (libpath) {
    library.dynam.unload("RStoolbox", libpath)
}

#' Can process in memory. Copied from raster package
#' @noRd
.canProcInMem <- function(x, n = 4, verbose = FALSE) {
  nc <- ncell(x)
  n <- n * dim(x)[3]
  memneed <- nc * n * 8
  if (memneed < .minmemory()) {
    if (verbose) {
      gb <- 1.07374e+09
      cat("            GB")
      cat(paste("\n   needed :", round(memneed / gb, 2)))
      cat("below minmemory threshold")
    }
    return(TRUE)
  }
  maxmem <- .maxmemory()
  memavail <- availableRAMCpp(maxmem)
  if (verbose) {
    gb <- 1.07374e+09
    cat("            GB")
    cat(paste("\navailable :", round(memavail / gb, 2)))
    cat(paste0("\n      ", round(100 * .memfrac()), "% : ", round(.memfrac() * memavail / gb, 2)))
    cat(paste("\n   needed :", round(memneed / gb, 2)))
    cat(paste("\n  allowed :", round(maxmem / gb, 2), " (if available)\n"))
  }
  if (nc > (2 ^ 31 - 1)) return(FALSE)
  memavail <- .memfrac() * memavail
  memavail <- min(memavail, maxmem)
  if (memneed > memavail) {
    options(rasterChunk = min(.chunksize(), memavail * 0.25))
    return(FALSE)
  } else {
    return(TRUE)
  }
}

.maxmemory <- function() {
	default <- 5e+9
	d <- getOption('rasterMaxMemory')
	if (is.null(d)) {
		return( default )
	}
	d <- round(as.numeric(d[1]))
	if (is.na(d) | d < 1e+6) {
		d <- 1e+6
	}
	return(d)
}

.minmemory <- function() {
	default <- 8e+6
	d <- getOption('rasterMinMemory')
	if (is.null(d)) {
		return( default )
	}
	d <- round(as.numeric(d[1]))
	if (is.na(d) | d < 10000) {
		d <- 8e+6
	}
	return(d)
}


.toDisk <- function(..., todisk) {
	if (missing(todisk)) {
		todisk <- getOption('rasterToDisk')
		if (is.null(todisk)) {
			return(FALSE)  # the default
		} else {
			try (todisk <- as.logical(todisk))
			if (is.logical(todisk)) {
				return(todisk)
			} else {
				return(FALSE)
			}
		}
	} else {
		if (is.logical(todisk)) {
			return(todisk)
		} else {
			return(FALSE)
		}
	}
}

.chunksize <- function(){
	default <- 10^8
	d <- getOption('rasterChunkSize')
	if (is.null(d)) {
		return( default )
	}
	d <- round(as.numeric(d[1]))
	if (is.na(d) | d < 10000) {
		d <- default
	}
	return(d)
}

.memfrac <- function() {
	default <- 0.6
	d <- getOption('rasterMemfrac')
	if (is.null(d)) {
		return( default )
	} else {
		return(d)
	}
}

.terraTmpFile <- function(prefix = "terra_tmp_", ext = ".tif") {
  # Create a temporary directory
  temp_dir <- file.path(tempdir(), "raster")

  # Ensure the directory exists
  if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)

  # Generate a unique file name
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  filename <- paste0(prefix, timestamp, "_", Sys.getpid(), "_", sample.int(99999, 1), ext)

  # Return the full path to the temporary file
  full_path <- file.path(temp_dir, filename)
  return(full_path)
}
