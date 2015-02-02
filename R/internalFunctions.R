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



