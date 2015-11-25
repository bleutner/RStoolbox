#' Import separate Landsat files into single stack
#' 
#' Reads Landsat MTL or XML metadata files and loads single Landsat Tiffs into a rasterStack.
#' Be aware that by default stackMeta() does NOT import panchromatic bands nor thermal bands with resolutions != 30m.
#' 
#' @param file Character. Path to Landsat MTL metadata (*_MTL.txt) file or an Landsat CDR xml metadata file (*.xml).
#' @param category Character vector. Which category of data to return. Options 'image': image data, 'pan': panchromatic image, 'index': multiband indices, 'qa' quality flag bands, 'all': all categories.
#' @param quantity Character vector. Which quantity should be returned. Options: digital numbers ('dn'), top of atmosphere reflectance ('tre'), at surface reflectance ('sre'), brightness temperature ('bt'), spectral index ('index'), all quantities ('all').
#' @param allResolutions Logical. if \code{TRUE} a list will be returned with length = unique spatial resolutions.
#' @return 
#' Returns one single RasterStack comprising all requested bands. 
#' If \code{allResolutions = TRUE} *and* there are different resolution layers (e.g. a 15m panchromatic band along wit 30m imagery) a list of RasterStacks will be returned.
#' @note 
#' Be aware that by default stackMeta() does NOT import panchromatic bands nor thermal bands with resolutions != 30m. Use the allResolutions argument to import all layers.
#' Note that nowadays the USGS uses cubic convolution to resample the TIR bands to 30m resolution.
#' @export 
#' @examples 
#' ## Example metadata file (MTL)
#' mtlFile  <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")
#' 
#' ## Read metadata
#' metaData <- readMeta(mtlFile)
#' summary(metaData)
#' 
#' ## Load rasters based on metadata file
#' lsat     <- stackMeta(mtlFile)
#' lsat
stackMeta <- function(file,  quantity = "all", category = "image", allResolutions = FALSE){ 
    
    stopifnot( !any(!category %in%  c("pan", "image", "index", "qa", "all")), !any(!quantity %in% c("all", "dn", "tra", "tre", "sre", "bt", "index")))
    
    ## Read metadata and extract layer file names
    meta <- if(!inherits(file, "ImageMetaData")){
                readMeta(file)
            } else {
                file 
            }
    files <- meta$DATA$FILES    
    metaFile <- meta$METADATA_FILE 
    
    if("all" %in% quantity) quantity <- unique(meta$DATA$QUANTITY) 
    if("all" %in% category) category <- unique(meta$DATA$CATEGORY)
    quantAvail <- quantity %in% meta$DATA$QUANTITY
    typAvail <- category %in% meta$DATA$CATEGORY
    if(sum(quantAvail)  == 0) stop("None of the specifed quantities exist according to the metadata. You specified:", paste0(quantity, collapse=", "), call.=FALSE )
    if(any(!quantAvail)) warning("The following specified quantities don't exist: ", paste0(quantity[!quantAvail], collapse=", ") ,"\nReturning available quantities:", paste0(quantity[quantAvail], collapse=", "), call.=FALSE)
    if(sum(typAvail)  == 0) stop("None of the specifed categories exists according to the metadata. You specified:", paste0(category, collapse=", "), call.=FALSE )
    if(any(!typAvail)) warning("The following specified categories don't exist: ", paste0(category[!typAvail], collapse=", ") ,"\nReturning available categories:", paste0(category[typAvail], collapse=", "), call.=FALSE)
    
    ## Load layers
    path  <- if(basename(metaFile) != metaFile)  gsub(basename(metaFile), "", metaFile) else NULL
    
    ## Select products to return 
    se <- meta$DATA$CATEGORY %in% category & meta$DATA$QUANTITY %in% quantity
    select <- meta$DATA$BANDS[se]               
    
    ## Import rasters
    rl <- lapply(paste0(path, files), raster)
    resL <- lapply(rl, function(x) res(x)[1])
    resLs <- resL[se]
    if(any(resL > 30)) .vMessage("Your Landsat data includes TIR band(s) which were not resampled to 30m.")
    if(length(unique(resLs)) > 1 & !allResolutions) warning("You asked to import rasters of different resolutions but the allResolutions argument is FALSE. Will return only 30m data", call. = FALSE)
    
    
    ## Return resolutions
    returnRes <- if(allResolutions) {
                unlist(unique(resLs))
            } else if(any(resLs == 30)) 30 else {
                min(unlist(resLs)) 
            } 
    
    ## Stack
    LS 	<- lapply(returnRes, function(x){
                s			<- stack(rl[resL == x])
                names(s) 	<- meta$DATA$BANDS[resL == x]
                s[[ which(names(s) %in% select)]]
            })
    LS[lapply(LS, nlayers) == 0] <- NULL
    names(LS) <- paste0("spatRes_",returnRes,"m")
    if(!allResolutions) LS <- LS[[1]]
    
    return(LS)
}
