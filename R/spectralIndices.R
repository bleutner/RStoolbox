#' Spectral indices
#' 
#' Calculate a suite of multispectral indices such as NDVI, SAVI etc. in an efficient way.
#' 
#' @param img Raster* object. Typically remote sensing imagery, which is to be classified.
#' @param blue Character or integer. Blue band. 
#' @param green Character or integer. Green band. 
#' @param red Character or integer. Red band. 
#' @param nir Character or integer. Near-infrared band (700-1100nm). 
#' @param swir1 Character or integer. Short-wave-infrared band (1100 1351nm). 
#' @param swir2 Character or integer. Short-wave-infrared band (1400-1800nm). 
#' @param indices Character. One or more spectral indices to calculate (see Details). By default (NULL) all implemented indices given the spectral bands which are provided will be calculated.
#' @param index Character. Alias for \code{indices}.
#' @param coefs List of coefficients (see Details).  
#' @param ... further arguments such as filename etc. passed to \link[raster]{writeRaster}
#' @return  RasterBrick or a RasterLayer if length(indices) == 1
#' @template spectralIndices_table 
#' @seealso \code{\link[raster]{overlay}} 
#' @export
#' @examples
#' library(ggplot2)
#' data(rlogo)
#' rlogo[["nir"]] <- rlogo[[2]] + 100
#' SI <- spectralIndices(rlogo, red = "red", nir = "nir", indices = "NDVI")
#' ggR(SI, geom_raster = TRUE) + 
#'     scale_fill_gradient(low = "white", high = "green", na.value = NA)
spectralIndices <- function(img,
        blue=NULL, green=NULL, red=NULL, nir=NULL, swir1 =NULL, swir2 = NULL, 
        indices=NULL, index = NULL, coefs = list(L = 0.5,  G = 2.5, L_evi = 1,  C1 = 6,  C2 = 7.5, s = 1),
        ... ) {
    # TODO: add further indices
    # TODO: soil line estimator
    # TODO: change swir2 to swir11 and swir1 2
    
	## We will use the following wavlength range definitions (following Schowengerdt 2007)
	# VIS   | Visible              |   400  -    700 nm
	# RED   |
	# GREEN |
	# BLUE  |
	# NIR   | Near infra-red       |   700  -   1100 nm
	# swir1 | Short-wave infra-red |  1100  -   1351 nm
	# swir2 | shortwave infra-red  |  1400  -   1800 nm
	# swir3 | Shortwave infra-red  |  2000  -   2500 nm
	# mir1  | midwave infra-red    |  3000  -   4000 nm 
	# mir2  | midwave infra-red    |  4500  -   5000 nm 
	# TIR1  | thermal infra-red    |  8000  -   9500 nm
	# TIR2  | thermal infra-red    | 10000  - 140000 nm
	##	
	
	
	if(!is.null(index)) indices <- index  ## argument translation for convenience
	
	
    
    ## Coefficients
    defaultCoefs <- list(L = 0.5,  G = 2.5, L_evi = 1,  C1 = 6,  C2 = 7.5, s = 1)     
    implem <- names(coefs) %in% names(defaultCoefs)
    if(any(!implem)) warning("Non-implemented coefficients are ignored: ", paste0(names(coefs)[!implem], collapse=", "),
                "\nimplemented coefficients are: ", paste0(names(defaultCoefs), collapse = ", "))
    list2env(c(coefs, defaultCoefs[!names(defaultCoefs) %in% names(coefs)]), envir = environment())
    
    ## Check indices
    ind <- if(is.null(indices)) names(BANDSdb) else toupper(indices)  
    if(!any(ind %in% names(BANDSdb))) stop("indices must either be NULL to calculate all indices",
                "\nor element of c(", paste0(names(BANDSdb),collapse=","),") for specific indices.", call. = FALSE)
    
    ## Gather function arguments (all provided bands) and create args
    potArgs  <- c("blue", "green", "red", "nir", "swir2", "swir1")
    actArgs  <- vapply(potArgs, function(x) !is.null(get(x)), logical(1))    
    bands    <- potArgs[actArgs]
    
    ## Subset calculated indices to possible based on band inputs and / or user request
    requested    <- BANDSdb[ind]
    
    canCalc  <- names(requested)[!vapply(requested, function(x) any(!x %in% bands), logical(1))]
    ind  <- ind[ind %in% canCalc]   
    if(!length(ind)) stop("No index could be calculated. At least for one index you must specify *all* required bands.",
                "\n  See ?spectralIndices for information on required bands per index.")
    if(length(ind) < length(indices)){
        not <- setdiff(indices,ind)
        notbands <- setdiff(unlist(requested[not]), bands)
        warning("The following indices were requested but cannot be calculated: ", not,
                "\n  because the following required bands were not specified: ",
                notbands,
                "\n  The remaining fully specified indices will be calculated.")
    }
    
    ## Get required designated bands
    retrieve   <- lapply(bands, get, envir=environment())
    bandsCalc  <- vapply(retrieve, function(xi) {  if(is.character(xi)) match(xi, names(img)) else xi  }, numeric(1))
    names(bandsCalc) <- bands       
    
    ## Adjust layer argument so that the first layer we use is now layer 1, etc.
    ## This way we don't have to sample the whole stack if we only need a few layers
    fullSet <- vapply(potArgs, function(n) match(n, names(bandsCalc)), integer(1))
    
    # Perform calculations 
    indexMagic <- .paraRasterFun(img[[bandsCalc]], rasterFun = raster::calc,
            args = list(fun = function(m) {
                        spectralIndicesCpp(
                                x = m,
                                indices   = canCalc,
                                blueBand  = fullSet[["blue"]],
                                greenBand = fullSet[["green"]], 
                                redBand   = fullSet[["red"]],
                                nirBand   = fullSet[["nir"]], 
                                swir1Band   = fullSet[["swir1"]], 
                                swir2Band  = fullSet[["swir2"]],
                                L = coefs[["L"]],  G = coefs[["G"]], Levi = coefs[["L_evi"]], 
                                C1 = coefs[["C1"]], C2 = coefs[["C2"]], s = coefs[["s"]]
                        )},
                    forcefun =TRUE), wrArgs = list(...))
    
    names(indexMagic) <- canCalc      
    
    return(indexMagic)
}


BANDSdb <-  list(               
        DVI  	= c("red", "nir"),
        EVI		=  c("red", "nir", "blue"),
        GEMI	=  c("red", "nir"),
        LSWI	=  c("red", "swir1"),
        MSAVI	=  c("red", "nir"),
        MSAVI2	=  c("red", "nir"),
        NDVI	=  c("red", "nir"),
        NDWI 	=  c("green", "nir"),
        SAVI    =  c("red", "nir"), 
		SATVI   =  c("red", "swir1", "swir2"),
        SLAVI	=  c("red", "nir", "swir2"),
        SR 		=  c("red", "nir"),     
        TVI 	=  c("red", "nir"),
        WDVI    =  c("red", "nir")
)


## NOT USED FOR CALCULATIONS
#' Database of spectral indices
#' @keywords internal
#' @noRd 
.IDXdb <-  list(               
        DVI	= function(red, nir) {s*nir-red},
        CTVI    = function(red, nir) {(nir-red)/(nir+red) + 0.5},
	EVI	= function(red, nir, blue) {G * ((nir - red) / (nir + C1 * red - C2 * blue + L_evi))},
        GEMI	= function(red, nir) {(((nir^2 - red^2) * 2 + (nir * 1.5) + (red * 0.5) ) / (nir + red + 0.5)) * (1 - ((((nir^2 - red^2) * 2 + (nir * 1.5) + (red * 0.5) ) / (nir + red + 0.5)) * 0.25)) - ((red - 0.125) / (1 - red))},
        LSWI	= function(nir, swir1) {(nir-swir1)/(nir+swir1)},
	MNDWI    = function(green, swir1) {(green-swir1) / (green+swir1)},
        MSAVI	= function(red, nir) {nir + 0.5 - (0.5 * sqrt((2 * nir + 1)^2 - 8 * (nir - (2 * red))))},
        MSAVI2	= function(red, nir) {(2 * (nir + 1) - sqrt((2 * nir + 1)^2 - 8 * (nir - red))) / 2},
   #     MSI     = function(nir, swir2) {swir2/nir},
        NBRI     = function(nir, swir2) { (nir - swir2) / (nir + swir2)},
        NDVI	= function(red, nir) {(nir-red)/(nir+red)}, 
        NDWI 	= function(green, nir) {(green - nir)/(green + nir)},
        NRVI    = function(red, nir) {(red/nir - 1)/(red/nir + 1)},
        RVI     = function(red, nir) {red/nir},
        SATVI   = function(red, swir1, swir2) {(swir1 - red) / (swir1 + red + L) * (1 + L) - (swir2 / 2)},
        SAVI    = function(red, nir) {(nir - red) * (1+L) / (nir + red + L)}, 
        SLAVI	= function(red, nir, swir2) {nir / (red + swir2)},
        SR 	= function(red, nir) {nir / red},     
        TVI 	= function(red, nir) {sqrt((nir-red)/(nir+red)+0.5)},
        TTVI    = function(red, nir) {sqrt(abs((nir-red)/(nir+red) + 0.5))},
        WDVI    = function(red, nir) {nir - s * red}
)

#' References and names to .IDXdb
#' @keywords internal
#' @noRd 
.IDX.REFdb <- list(
        DVI     = c("Richardson1977", "Difference Vegetation Index") ,
        CTVI    = c("Perry1984", "Corrected Transformed Vegetation Index"),
        EVI     = c("Huete1999", "Enhanced Vegetation Index"),
        GEMI    = c("Pinty1992","Global Environmental Monitoring Index"),
        LSWI    = c("Xiao2004", "Land Surface Water Index"),
        MSAVI	= c("Qi1994","Modified Soil Adjusted Vegetation Index"),
        MSAVI2	= c("Qi1994","Modified Soil Adjusted Vegetation Index 2"),
        MNDWI   = c("", "Modified Normalised Difference Water Index"),
   #     MSI     = c()       
        NBRI    = c("", "Normalised Burn Ratio Index"),
	NDVI	= c("Rouse1974", "Normalised Difference Vegetation Index"),
        NDWI	= c("Gao1996", "Normalised Difference Water Index"),
        NRVI    = c("Baret1991","Normalised Ratio Vegetation Index"),
        RVI     = c("", "Ratio Vegetation Index"),
        SATVI   = c("", "Soil Adjusted Total Vegetation Index"),
        SAVI    = c("Huete1988", "Soil Adjusted Vegetation Index"),
        SLAVI	= c("Lymburger2000","Specific Leaf Area Vegetation Index"),
        SR      = c("Birth1968", "Simple Ratio Vegetation Index"),  #or Jordan1969
        TVI 	= c("Deering1975","Transformed Vegetation Index"),
        TTVI    = c("Thiam1997", "Thiam's Transformed Vegetation Index"),
        WDVI    = c("Richardson1977","Weighted Difference Vegetation Index")
)







