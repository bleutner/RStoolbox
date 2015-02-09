#' Spectral indices
#' 
#' Calculate a suite of multispectral indices such as NDVI, SAVI etc. in an efficient way.
#' 
#' @param inputRaster Raster* object. Typically remote sensing imagery, which is to be classified.
#' @param blue Character or integer. Blue band. 
#' @param red Character or integer. Red band. 
#' @param nir Character or integer. Near-infrared band. 
#' @param mir Character or integer. Midwave-infrared band. 
#' @param indices Character. One or more spectral indices to calculate (see Details). By default (NULL) all implemented indices given the spectral bands which are provided will be calculated.
#' @param coefs List of coefficients (see Details).  
#' @param ... further arguments such as filename etc. passed to \link[raster]{writeRaster}
#' @return  RasterBrick or a RasterLayer if length(indices) == 1
#' @template spectralIndices_table 
#' @seealso \code{\link[raster]{overlay}} 
#' @export
#' @examples
#' r <- raster(ncol=10,nrow=10)
#' r[] <- sample(1:155, 100, TRUE)
#' r <- stack(r, r + 90 + rnorm(100, 10)) 
#' names(r) <- c("red", "nir")
#' SI <- spectralIndices(r, red = 1, nir = 2, indices = "NDVI")
#' plot(SI)
spectralIndices <- function(inputRaster, blue=NULL, red=NULL, nir=NULL, mir=NULL, indices=NULL, coefs = list(L = 0.5,  G = 2.5, L_evi = 1,  C1 = 6,  C2 = 7.5),
        ... ) {
    # TODO: add further indices
    
    ## Coefficients
    defaultCoefs <- list(L = 0.5,  G = 2.5, L_evi = 1,  C1 = 6,  C2 = 7.5)     
    implem <- names(coefs) %in% names(defaultCoefs)
    if(any(!implem)) warning("Non-implemented coefficients are ignored: ", paste0(names(coefs)[!implem], collapse=", "),
                "\nimplemented coefficients are: ", paste0(names(defaultCoefs), collapse = ", "))
    list2env(c(coefs, defaultCoefs[!names(defaultCoefs) %in% names(coefs)]), envir = environment())
    
    ## Check indices
    ind <- if(is.null(indices)) names(.IDXdb) else toupper(indices)  
    if(!any(ind %in% names(.IDXdb))) stop("indices must either be NULL to calculate all indices",
                "\nor element of c(", paste0(names(.IDXdb),collapse=","),") for specific indices.", call. = FALSE)
    
    ## Gather function arguments (all provided bands) and create args
    args    <- alist(blue=, red=, nir=, mir=)
    potArgs <- names(args) ## potential bands
    actArgs <- vapply(potArgs, function(x) is.null(get(x)), logical(1))
    args[actArgs]  <- NULL      ## keep only provided args      
    # args    <- as.pairlist(c(args, alist(...=))) 
    args    <- as.pairlist(args)  
    bands <- names(actArgs)[!actArgs]
    
    ## Subset calculated indices to possible based on band inputs and / or user request
    frmls    <- lapply(lapply(.IDXdb, formals), names)
    canCalc  <- names(frmls)[!vapply(frmls, function(x) any(!x %in% bands), logical(1))]
    ind  <- ind[ind %in% canCalc]   
    if(length(ind) == 0) stop("No index could be calculated. At least for one index you must specify *all* required bands.",
                "\n  See ?spectralIndices for information on required bands per index.")
    if(length(ind) < length(indices)){
        not <- setdiff(indices,ind)
        notbands <- setdiff(unlist(frmls[not]), bands)
        warning("The following indices were requested but cannot be calculated: ", not,
                "\n  To following required bands are not specified: ",
                notbands,
                "\n  The remaining fully specified indices are calculated nevertheless.")
    }
    
    ## Finally make functions with args and bodies
    bdys  <- lapply(.IDXdb[ind], body) ## get the bodies   
    funSlaves  <- lapply(bdys, function(x) eval(call("function", args=args , body = x))) ## make functions     
    funMaster <- function(...){
        sapply(funSlaves, function(f, ...) f(...), ...)
    } 
    
    ## Get designated bands
    bands <- as.list(environment())[bands]
    ## Treat mixture of character and integer band assignment
    if(is.list(bands)){
        chr <- sapply(bands, is.character)
        bands[chr] <- match(bands[chr], names(inputRaster))
        bands <- unlist(bands)
    }
    
    # Perform calculations (each pixel must be read only once due to the function assembly above)
    # this should save some significant time for large Rasters   
    indexMagic    <- overlay(inputRaster[[bands]], fun = funMaster, ...)
    names(indexMagic) <- names(bdys)      
    
    return(indexMagic)
}



#' Database of spectral indices
#' @keywords internal
.IDXdb <-  list(               
        DVI		= function(red, nir) {s*nir-red},
        EVI		= function(red, nir, blue) {G * ((nir - red) / (nir + C1 * red - C2 * blue + L_evi))},
        GEMI	= function(red, nir) {(((nir^2 - red^2) * 2 + (nir * 1.5) + (red * 0.5) ) / (nir + red + 0.5)) * (1 - ((((nir^2 - red^2) * 2 + (nir * 1.5) + (red * 0.5) ) / (nir + red + 0.5)) * 0.25)) - ((red - 0.125) / (1 - red))},
        LSWI	= function(nir, swir) {(nir-swir)/(nir+swir)},
        MSAVI	= function(red, nir) {nir + 0.5 - (0.5 * sqrt((2 * nir + 1)^2 - 8 * (nir - (2 * red))))},
        MSAVI2	= function(red, nir) {(2 * (nir + 1) - sqrt((2 * nir + 1)^2 - 8 * (nir - red))) / 2},
        NDVI	= function(red, nir) {(nir-red)/(nir+red)}, 
        NDWI 	= function(green, nir) {(green - nir)/(green + nir)},
        SAVI    = function(red, nir) {(nir - red) * (1+L) / (nir + red + L)}, 
        SLAVI	= function(red, nir, mir) {nir / (red + mir)},
        SR 		= function(red, nir) {nir / red},     
        TVI 	= function(red, nir) {sqrt(((nir-red)/(nir+red))+0.5)},
        #CTVI	= function(red, nir) {}
        WDVI    = function(red, nir) {nir - s * red}
)

#' References and names to .IDXdb
#' @keywords internal
.IDX.REFdb <- list(
        DVI     = c("Richardson1977", "Difference Vegetation Index") ,
        EVI     = c("Huete1999", "Enhanced Vegetation Index"),
        GEMI    = c("Pinty1992","Global Environmental Monitoring Index"),
        LSWI    = c("", "Land Surface Water Index"),
        MSAVI	= c("Qi1994","Modified Soil Adjusted Vegetation Index"),
        MSAVI2	= c("Qi1994","Modified Soil Adjusted Vegetation Index 2"),
        NDVI	= c("Rouse1974", "Normalised Difference Vegetation Index"),
        NDWI	= c("", "Normalised Difference Water Index"),
        SAVI    = c("Huete1988", "Soil Adjusted Vegetation Index"),
        SLAVI	= c("Lymburger2000","Specific Leaf Area Vegetation Index"),
        SR 		= c("Birth1968", "Simple Ratio Vegetation Index"),  #or Jordan1969
        TVI 	= c("Deering1975","Transformed Vegetation Index"),
        WDVI    = c("Richardson1977","Weighted Difference Vegetation Index")
)







