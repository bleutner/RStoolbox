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

    .IDXDB <- list(
        CLG = list(c("Gitelson2003", "Green-band Chlorophyll Index"),
                function(redEdge3, green) {redEdge3/green - 1}),
        CLRE = list(c("Gitelson2003", "Red-edge-band Chlorophyll Index"),
                function(redEdge3, redEdge1) {redEdge3/redEdge1 - 1}),
        CTVI = list(c("Perry1984", "Corrected Transformed Vegetation Index"),
                function(red, nir) {(NDVI+.5)/sqrt(abs(NDVI+.5))} ),
        DVI = list(c("Richardson1977", "Difference Vegetation Index"),
                function(red, nir) {s*nir-red}),
        EVI = list(c("Huete1999", "Enhanced Vegetation Index"),
                function(red, nir, blue) {G * ((nir - red) / (nir + C1 * red - C2 * blue + L_evi))}),
        EVI2 = list(c("Jiang 2008", "Two-band Enhanced Vegetation Index"),
                function(red, nir) {G * (nir-red)/(nir + 2.4*red +1)}),
        GEMI = list(c("Pinty1992", "Global Environmental Monitoring Index"),
                function(red, nir) {(((nir^2 - red^2) * 2 + (nir * 1.5) + (red * 0.5) ) / (nir + red + 0.5)) * (1 - ((((nir^2 - red^2) * 2 + (nir * 1.5) + (red * 0.5) ) / (nir + red + 0.5)) * 0.25)) - ((red - 0.125) / (1 - red))}),
        GNDVI = list(c("Gitelson1998", "Green Normalised Difference Vegetation Index" ),
                function(green, nir) {(nir-green)/(nir+green)}),
		KNDVI = list(c("Camps-Valls2021", "Kernel Normalised Difference Vegetation Index"),
				function(red, nir) {tanh(((nir-red)/(nir+red)))^2}),
        MCARI = list(c("Daughtery2000", "Modified Chlorophyll Absorption Ratio Index" ),
                function(green, red, redEdge1) {((redEdge1-red)-(redEdge1-green))*(redEdge1/red)}),
        MNDWI = list(c("Xu2006", "Modified Normalised Difference Water Index"),
                function(green, swir2) {(green-swir2) / (green+swir2)}),
        MSAVI = list(c("Qi1994", "Modified Soil Adjusted Vegetation Index" ),
                function(red, nir) {nir + 0.5 - (0.5 * sqrt((2 * nir + 1)^2 - 8 * (nir - (2 * red))))}),
        MSAVI2 = list(c("Qi1994", "Modified Soil Adjusted Vegetation Index 2" ),
                function(red, nir) {(2 * (nir + 1) - sqrt((2 * nir + 1)^2 - 8 * (nir - red))) / 2}),
        MTCI = list(c("DashAndCurran2004", "MERIS Terrestrial Chlorophyll Index"),
                function(red, redEdge1, redEdge2) {(redEdge2-redEdge1)/(redEdge1-red)}),
        NBRI = list(c("Garcia1991", "Normalised Burn Ratio Index"),
                function(nir, swir3) { (nir - swir3) / (nir + swir3)}),
        NDREI1 = list(c("GitelsonAndMerzlyak1994", "Normalised Difference Red Edge Index 1"),
                function(redEdge2, redEdge1) {(redEdge2-redEdge1)/(redEdge2+redEdge1)}),
        NDREI2 = list(c("Barnes2000", "Normalised Difference Red Edge Index 2"),
                function(redEdge3, redEdge1) {(redEdge3-redEdge1)/(redEdge3+redEdge1)}),
        NDVI = list(c("Rouse1974", "Normalised Difference Vegetation Index"),
                function(red, nir) {(nir-red)/(nir+red)}),
        NDVIC = list(c("Nemani1993", "Corrected Normalised Difference Vegetation Index" ),
                function(red, nir, swir2) {(nir-red)/(nir+red)*(1-((swir2 - swir2ccc)/(swir2coc-swir2ccc)))}),
        NDWI = list(c("McFeeters1996", "Normalised Difference Water Index"),
                function(green, nir) {(green - nir)/(green + nir)}),
        NDWI2 = list(c("Gao1996", "Normalised Difference Water Index"),
                function(nir, swir2) {(nir - swir2)/(nir + swir2)}),
        NRVI = list(c("Baret1991", "Normalised Ratio Vegetation Index" ),
                function(red, nir) {(red/nir - 1)/(red/nir + 1)}),
        REIP = list(c("GuyotAndBarnet1988", "Red Edge Inflection Point"),
                function(red, redEdge1, redEdge2, redEdge3) {0.705+0.35*((red+redEdge3)/(2-redEdge1))/(redEdge2-redEdge1)}),
        RVI = list(c("", "Ratio Vegetation Index"),
                function(red, nir) {red/nir}),
        SATVI = list(c("Marsett2006", "Soil Adjusted Total Vegetation Index"),
                function(red, swir2, swir3) {(swir2 - red) / (swir2 + red + L) * (1 + L) - (swir3 / 2)}),
        SAVI = list(c("Huete1988", "Soil Adjusted Vegetation Index"),
                function(red, nir) {(nir - red) * (1+L) / (nir + red + L)}),
        SLAVI = list(c("Lymburger2000", "Specific Leaf Area Vegetation Index"),
                function(red, nir, swir2) {nir / (red + swir2)}),
        SR = list(c("Birth1968", "Simple Ratio Vegetation Index"),
                function(red, nir) {nir / red}),
        TTVI = list(c("Thiam1997", "Thiam's Transformed Vegetation Index"),
                function(red, nir) {sqrt(abs((nir-red)/(nir+red) + 0.5))}),
        TVI = list(c("Deering1975", "Transformed Vegetation Index"),
                function(red, nir) {sqrt((nir-red)/(nir+red)+0.5)}),
        WDVI = list(c("Richardson1977", "Weighted Difference Vegetation Index"),
                function(red, nir) {nir - s * red}),
        CUSTOM = list(c("Mueller2024", "Super custom index"),
                      function(red) {blue + red}),
        CUSTOM2 = list(c("Mueller2024", "Super custom index 2"),
                      function(red) {red * red})
    )

    if(is.null(getOption("RStoolbox.verbose")))  options(RStoolbox.verbose = FALSE)
    if(is.null(getOption("RStoolbox.idxdb")))  options(RStoolbox.idxdb = .IDXDB)
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


#' Init spectralIndices within the index DB
#'
#' will restore global options after function has been called
#' @param spectralIndices List
#' @keywords internal
#' @noRd
.initIDXdb <- function(idxdb){
    idxbg <- force(getOption("RStoolbox.idxdb"))
    do.call("on.exit", list(substitute(options(RStoolbox.idxdb = idxbg))), envir=parent.frame())
    options(RStoolbox.idxdb = idxdb)
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

.with_env <- function(f) {
    stopifnot(is.function(f))
    environment(f) <- new.env()
    f
}
