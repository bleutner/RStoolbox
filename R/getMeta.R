#' Extract bandwise information from ImageMetaData
#' 
#' This is an accessor function to quickly access information stored in ImageMetaData, e.g. scale factor per band.
#' Intended for use with imagery which was imported using stackMeta. Will return parameters using the actual band order in img.
#' 
#' @param img Raster* or character vector with band names. 
#' @param metaData ImageMetaData or path to meta data file.
#' @param what Character. Parameter to extract. Either data descriptors, or conversion parameters (see Details for options)
#' @export 
#' @return 
#' If \code{what} is one of \code{c('CALRAD', 'CALBT', 'CALREF')} a data.frame is returned with bands in rows (order corresponding to \code{img} band order). 
#' Otherwise a named numeric vector with the corresponding parameter is returned (layernames as names). 
#' @details 
#' Possible metadata parameters (\code{what} argument):
#' 
#' Data descriptors
#' \tabular{ll}{
#' 'FILES' \tab  \cr
#' 'QUANTITY' \tab \cr
#' 'CATEGORY' \tab \cr
#' 'NA_VALUE' \tab \cr
#' 'SATURATE_VALUE' \tab \cr 
#' 'SCALE_FACTOR' \tab \cr
#' 'DATA_TYPE' \tab \cr
#' 'SPATIAL_RESOLUTION' \tab \cr
#' }
#' Conversion parameters
#' \tabular{ll}{
#' 'CALRAD' \tab Conversion parameters from DN to radiance \cr
#' 'CALBT' \tab Conversion parameters from radiance to brightness temperature \cr
#' 'CALREF' \tab Conversion parameters from DN to reflectance (Landsat 8 only) \cr
#' }
#' @examples 
#' ## Import example data
#' mtlFile  <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")
#' meta <- readMeta(mtlFile)
#' lsat <- stackMeta(mtlFile)
#' 
#' ## Get integer scale factors
#' getMeta(lsat, metaData = meta, what = "SCALE_FACTOR")
#' 
#' ## Conversion factors for brightness temperature
#' getMeta("B6_dn", metaData = meta, what = "CALBT")
#' 
#' ## Conversion factors to top-of-atmosphere radiance
#' ## Band order not corresponding to metaData order
#' getMeta(lsat[[5:1]], metaData = meta, what = "CALRAD")
#' 
#' ## Get integer scale factors
#' getMeta(lsat, metaData = meta, what = "SCALE_FACTOR")
#' 
#' ## Get file basenames
#' getMeta(lsat, metaData = meta, what = "FILES")
#' 
getMeta <- function(img, metaData, what){
   
		
    if(inherits(metaData, "character")) {
        metaData <- readMeta(metaData)
    } else if(!inherits(metaData, "ImageMetaData")){
        stop("metaData must be character or ImageMetaData")
    }
	
   	stopifnot(what %in% c(names(metaData$DATA), "CALREF", "CALRAD", "CALBT") & length(what) == 1)

    if(inherits(img, "Raster")){
        bds <- names(img) 
    } else if (inherits(img,"character")) {
        bds <- img
    } else {
        stop("img must be a Raster* or character")
    }
    
    if(what %in% c("CALREF", "CALRAD", "CALBT")) {
		if(length(metaData[[what]]) == 1) stop(paste(what, "is not populated in metaData"))
		present <- bds %in% rownames(metaData[[what]])		
		if (any(!present)) stop("Bands ", paste0(bds[!present], collapse = ", ") , " are not present in metaData.\n",
					"Available bands: ", rownames(metaData[[what]])	, collapse = ", ")
		out <- metaData[[what]][bds,]
	} else {
		present <- bds %in% rownames(metaData$DATA)
	    if (any(!present)) stop("Bands ", paste0(bds[!present], collapse = ", ") , " are not present in metaData.\n",
				"Available bands: ", paste0(rownames(metaData$DATA), collapse = ", "))
        out <- metaData$DATA[bds,what]
		names(out) <- bds
	}
	
	return(out)
    
}

