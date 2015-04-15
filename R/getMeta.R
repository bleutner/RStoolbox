#' Extract bandwise information from ImageMetaData
#' 
#' This is an accessor function to quickly access information stored in ImageMetaData, e.g. scale factor per band.
#' Intended for use with imagery which was imported using stackMeta.
#' 
#' @param img Raster* or character vector with band names. 
#' @param metaData ImageMetaData or path to meta data file.
#' @param what Character. Slot to extract.
#' @export 
getMeta <- function(img, metaData, what){
   
    if(inherits(metaData, "character")) {
        metaData <- readMeta(metaData)
    } else if(!inherits(metaData, "ImageMetaData")){
        stop("metaData must be character or ImageMetaData")
    }
   
    if(inherits(img, "Raster")){
        bds <- names(img) 
    } else if (inherits(img,"character")) {
        bds <- img
        present <- bds %in% rownames(metaData$DATA)
        if (any(!present)) stop("Bands ", paste0(bds[!present], collapse = ", ") , " are not present in metaData.\n",
                    "Available bands: ", paste0(rownames(metaData$DATA), collapse = ", "))
    } else {
        stop("img must be a Raster* or character")
    }
    stopifnot(what %in% names(metaData$DATA))
   
    return(metaData[[DATA]][bds,what])
    
}