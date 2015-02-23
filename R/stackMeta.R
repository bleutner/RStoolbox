#' Import separate Landsat files into single stack
#' 
#' Reads Landsat MTL or XML metadata files and loads single Landsat Tiffs into a rasterStack.
#' Be aware that by default stackLS() does NOT import panchromatic bands nor thermal bands with resolutions != 30m.
#' 
#' @param file character. Path to Landsat MTL metadata file (not an XML file!).
#' @param allResolutions logical. if \code{TRUE} a list will be returned with length = unique spatial resolutions.
#' @param category character vector. Which category of data to return. Options 'image': image data, 'pan': panchromatic image, 'index': multiband indices, 'qa' quality flag bands, 'all': all categories.
#' @param product Character vector. Which products should be returned. Options: digital numbers ('dn'), top of atmosphere reflectance ('tre'), at surface reflectance ('sre'), brightness temperature ('bt'), spectral index ('idx').
#' @return Either a list of rasterStacks comprising all resolutions or only one rasterStack comprising only 30m resolution imagery
#' @note 
#' Be aware that by default stackLS() does NOT import panchromatic bands nor thermal bands with resolutions != 30m. Use the allResolutions argument to import all layers.
#' Note that nowadays the USGS uses cubic convolution to resample the TIR bands to 30m resolution.
#' @export 
stackMeta <- function(file, allResolutions = FALSE, product = "all", category = "image"){ 
    ## TODO: check arguments
    stopifnot( !any(!category %in%  c("pan", "image", "index", "qa", "all")), !any(!product %in% c("all", "dn", "tra", "tre", "sre", "bt", "idx")))
    
    ## Read metadata and extract layer file names
    meta <- if(!inherits(file, "ImgMetaData")){
                readMeta(file)
            } else {
                file
            }
    files <- meta$DATA$FILES   
    file <- meta$METADATA_FILE
    
    if("all" %in% product) product <- unique(meta$DATA$PRODUCT)
    if("all" %in% category) category <- unique(meta$DATA$CATEGORY)
    proAvail <- product %in% meta$DATA$PRODUCT
    typAvail <- category %in% meta$DATA$CATEGORY
    if(sum(proAvail)  == 0) stop("None of the specifed products exist according to the metadata. You specified:", paste0(product, collapse=", "), call.=FALSE )
    if(any(!proAvail)) warning("The following specified products don't exist: ", paste0(product[!proAvail], collapse=", ") ,"\nReturning available products:", paste0(product[proAvail], collapse=", "), call.=FALSE)
    if(sum(typAvail)  == 0) stop("None of the specifed categories exists according to the metadata. You specified:", paste0(category, collapse=", "), call.=FALSE )
    if(any(!typAvail)) warning("The following specified categories don't exist: ", paste0(category[!typAvail], collapse=", ") ,"\nReturning available categories:", paste0(category[typAvail], collapse=", "), call.=FALSE)
    
    ## Load layers
    path  <- if(basename(file) != file)  gsub(basename(file), "", file) else NULL
    
    ## Import rasters
    rl <- lapply(paste0(path, files), raster)
    resL <- lapply(rl, function(x) res(x)[1])
    
    if(any(resL > 30)) message("Your Landsat data includes TIR band(s) which were not resampled to 30m.")
    
    ## Stack
    returnRes <- if(allResolutions) unlist(unique(resL)) else 30
    
    ## Select products to return
    select <- meta$DATA$BANDS[
            meta$DATA$CATEGORY %in% category &
                    meta$DATA$PRODUCT %in% product
    ]               
    
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
