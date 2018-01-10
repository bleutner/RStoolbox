#' Spectral Indices
#' 
#' Calculate a suite of multispectral indices such as NDVI, SAVI etc. in an efficient way.
#' 
#' @param img Raster* object. Typically remote sensing imagery, which is to be classified.
#' @param blue Character or integer. Blue band. 
#' @param green Character or integer. Green band. 
#' @param red Character or integer. Red band. 
#' @param nir Character or integer. Near-infrared band (700-1100nm). 
#' @param swir1 [temporarily deprecated]
#' @param swir2 Character or integer. Short-wave-infrared band (1400-1800nm). 
#' @param swir3 Character or integer. Short-wave-infrared band (2000-2500nm). 
#' @param indices Character. One or more spectral indices to calculate (see Details). By default (NULL) all implemented indices given the spectral bands which are provided will be calculated.
#' @param index Character. Alias for \code{indices}.
#' @param maskLayer RasterLayer containing a mask, e.g. clouds, for which pixels are set to NA. Alternatively a layername or -number can be provided if the mask is part of \code{img}. 
#' @param maskValue Integer. Pixel value in \code{maskLayer} which should be masked in output, i.e. will be set to \code{NA} in all calculated indices.
#' @param scaleFactor Numeric. Scale factor for the conversion of scaled reflectances to [0,1] value range (applied as reflectance/scaleFactor) Neccesary for calculating EVI/EVI2 with scaled reflectance values.
#' @param skipRefCheck Logical. When EVI/EVI2 is to be calculated there is a rough heuristic check, whether the data are inside [0,1]+/-0.5 (after applying a potential \code{scaleFactor}).
#'  If there are invalid reflectances, e.g. clouds with reflectance > 1 this check will result in a false positive and skip EVI calculation. Use this argument to skip this check in such cases *iff* you are sure the data and scaleFactor are valid. 
#' @param coefs List of coefficients (see Details).  
#' @param ... further arguments such as filename etc. passed to \link[raster]{writeRaster}
#' @return  RasterBrick or a RasterLayer if length(indices) == 1
#' @template spectralIndices_table 
#' @export
#' @examples
#' library(ggplot2)
#' library(raster)
#' data(lsat)
#' 
#' ## Calculate NDVI
#' ndvi <- spectralIndices(lsat, red = "B3_dn", nir = "B4_dn", indices = "NDVI")
#' ndvi
#' ggR(ndvi, geom_raster = TRUE) +
#'         scale_fill_gradientn(colours = c("black", "white")) 
#' 
#' ## Calculate all possible indices, given the provided bands 
#' ## Convert DNs to reflectance (required to calculate EVI and EVI2)
#' mtlFile  <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")
#' lsat_ref <- radCor(lsat, mtlFile, method = "apref")
#' 
#' SI <- spectralIndices(lsat_ref, red = "B3_tre", nir = "B4_tre")
#' plot(SI)
spectralIndices <- function(img,
        blue=NULL, green=NULL, red=NULL, nir = NULL, swir1 = NULL, swir2 =NULL, swir3 = NULL,
        scaleFactor = 1, skipRefCheck = FALSE,   indices=NULL, index = NULL, maskLayer = NULL, maskValue = 1,
        coefs = list(L = 0.5,  G = 2.5, L_evi = 1,  C1 = 6,  C2 = 7.5, s = 1, swir2ccc = NULL, swir2coc = NULL),
        ... ) {
    # TODO: soil line estimator
    
    ## We will use the following wavlength range definitions (following Schowengerdt 2007, p 10)
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
    if("LSWI" %in% toupper(indices)) stop("LSWI has been deprecated. Use NDWI2 instead; it is identical.")
    if(!is.null(swir1)) stop(paste0("Currently there is no spectral index requiring swir1.", 
                        "\nIn case you were using spectralIndices from a previous version of RStoolbox with the swir1 argument,",
                        "\nplease note that there has been naming correction. Former swir1 is now swir2 and former swir2 is now swir3.",
                        "\nsee: news(package='RStoolbox')"), call.=FALSE)
    
    ## Coefficients
    defaultCoefs <- list(L = 0.5,  G = 2.5, L_evi = 1,  C1 = 6,  C2 = 7.5, s = 1, swir2ccc = NULL, swir2coc = NULL)     
    implem <- names(coefs) %in% names(defaultCoefs)
    if(any(!implem)) warning("Non-implemented coefficients are ignored: ", paste0(names(coefs)[!implem], collapse=", "),
                "\nimplemented coefficients are: ", paste0(names(defaultCoefs), collapse = ", "))
    coefs <- c(coefs, defaultCoefs[setdiff(names(defaultCoefs), names(coefs))])
    
    ## Check indices
    ind <- if(is.null(indices)) names(BANDSdb) else toupper(indices)  
    if((is.null(coefs$swir2ccc) | is.null(coefs$swir2coc))) {
        if(!is.null(indices) & ("NDVIC" %in% ind)) warning("NDVIc can only be calculated if swir2ccc and swir2coc coefficients are provided.")  
        coefs$swir2ccc <- 0  ## dummy, cant pass NULL to spectralIndicesCpp
        coefs$swir2coc <- 1  ## dummy, cant pass NULL to spectralIndicesCpp
        ind <- setdiff(ind, "NDVIC")
    }
    if(!any(ind %in% names(BANDSdb))) stop("indices must either be NULL to calculate all indices",
                "\nor element of c(", paste0(names(BANDSdb),collapse=","),") for specific indices.", call. = FALSE)
    coefs$swir2cdiff <- coefs$swir2coc - coefs$swir2ccc 
    if(coefs$swir2cdiff <= 0) stop("NDVIc coefficient swir2ccc (completeley closed canopy) must be smaller than swir2coc (completely open canopy)")
    
    ## Gather function arguments (all provided bands) and create args
    potArgs  <- c("blue", "green", "red", "nir", "swir1", "swir2", "swir3")
    actArgs  <- vapply(potArgs, function(x) !is.null(get(x)), logical(1))    
    bands    <- potArgs[actArgs]
    
    ## Subset calculated indices to possible based on band inputs and / or user request
    requested    <- BANDSdb[ind]
    
    canCalc  <- names(requested)[!vapply(requested, function(x) any(!x %in% bands), logical(1))]
    if(!skipRefCheck && any(c("EVI","EVI2") %in% canCalc)){
        ## TODO: clarify setMinMax reqiurements throughout RStoolbox
        if(!.hasMinMax(img[[red]])) img[[red]] <- setMinMax(img[[red]])
        if((maxValue(img[[red]])/scaleFactor > 1.5 | minValue(img[[red]])/scaleFactor < -.5)){
            ## checking for range [0,1] +/- .5 to allow for artifacts in reflectance.
            warning("EVI/EVI2 parameters L_evi, G, C1 and C2 are defined for reflectance [0,1] but img values are outside of this range.\n",
                    "  If you are using scaled reflectance values please provide the scaleFactor argument.\n", 
                    "  If img is in DN or radiance it must be converted to reflectance.\n",
                    "  Skipping EVI calculation.\n")
            canCalc <- canCalc[!canCalc %in% c("EVI", "EVI2")]
            indices <- indices[!indices %in% c("EVI", "EVI2")]
        }
    }
    
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
    
    ## Add mask layer to img if present
    if(!is.null(maskLayer)){
        if(inherits(maskLayer, "Raster")) {
            if(nlayers(maskLayer) > 1) warning(sprintf("maskLayer has %s layers. Only the first layer will be used for masking.", nlayers(maskLayer)),call.=FALSE)
            img <- stack(img, maskLayer[[1]])
            names(img)[nlayers(img)] <- "mask"
        } else if (is.numeric(maskLayer)) {
            names(img)[maskLayer] <- "mask"
        }  else if (is.character(maskLayer)) {
            if(!maskLayer %in% names(img)) stop(paste0(maskLayer, "is not a layer in img.\nAvailable layers are: ", paste(names(img))), call. =FALSE)
            names(img)[names(img) == maskLayer] <- "mask"
        } else {
            stop("maskLayer must be NULL, a RasterLayer, a layer name or a layer number", call. =FALSE)
        }
        bandsCalc[["mask"]] <- which(names(img) == "mask") 
    }
    potArgs <- c(potArgs, "mask")

    
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
                                swir1Band  = fullSet[["swir1"]],                             
                                swir2Band  = fullSet[["swir2"]],
                                swir3Band  = fullSet[["swir3"]],
                                maskLayer = fullSet[["mask"]],
                                maskValue = maskValue,
                                L = coefs[["L"]],  G = coefs[["G"]], Levi = coefs[["L_evi"]], 
                                C1 = coefs[["C1"]], C2 = coefs[["C2"]], s = coefs[["s"]],
                                swir2ccc = coefs[["swir2ccc"]], swir2cdiff = coefs[["swir2cdiff"]], sf = scaleFactor
                        )},
                    forcefun =TRUE), wrArgs = list(...))
    
    names(indexMagic) <- canCalc      
    
    return(indexMagic)
}



## NOT USED FOR CALCULATIONS ONLY FOR DOCUMENTATION
## SEE /src/spectraIndices.cpp for calculations
#' Database of spectral indices
#' @keywords internal
#' @noRd 
.IDXdb <-  list(     
        CTVI    = function(red, nir) {(NDVI+.5)/sqrt(abs(NDVI+.5))},
        DVI     = function(red, nir) {s*nir-red},
        EVI      = function(red, nir, blue) {G * ((nir - red) / (nir + C1 * red - C2 * blue + L_evi))},
        EVI2    = function(red, nir) {G * (nir-red)/(nir + 2.4*red +1)},
        GEMI    = function(red, nir) {(((nir^2 - red^2) * 2 + (nir * 1.5) + (red * 0.5) ) / (nir + red + 0.5)) * (1 - ((((nir^2 - red^2) * 2 + (nir * 1.5) + (red * 0.5) ) / (nir + red + 0.5)) * 0.25)) - ((red - 0.125) / (1 - red))},
        GNDVI    = function(green, nir) {(nir-green)/(nir+green)}, 
        MNDWI   = function(green, swir2) {(green-swir2) / (green+swir2)},
        MSAVI    = function(red, nir) {nir + 0.5 - (0.5 * sqrt((2 * nir + 1)^2 - 8 * (nir - (2 * red))))},
        MSAVI2    = function(red, nir) {(2 * (nir + 1) - sqrt((2 * nir + 1)^2 - 8 * (nir - red))) / 2},
        NBRI    = function(nir, swir3) { (nir - swir3) / (nir + swir3)},
        NDVI    = function(red, nir) {(nir-red)/(nir+red)}, 
        NDVIC   = function(red, nir, swir2) {(nir-red)/(nir+red)*(1-((swir2 - swir2ccc)/(swir2coc-swir2ccc)))},
        NDWI     = function(green, nir) {(green - nir)/(green + nir)},
        NDWI2    = function(nir, swir2) {(nir - swir2)/(nir + swir2)},    
        NRVI    = function(red, nir) {(red/nir - 1)/(red/nir + 1)},
        RVI     = function(red, nir) {red/nir},
        SATVI   = function(red, swir2, swir3) {(swir2 - red) / (swir2 + red + L) * (1 + L) - (swir3 / 2)},
        SAVI    = function(red, nir) {(nir - red) * (1+L) / (nir + red + L)}, 
        SLAVI    = function(red, nir, swir2) {nir / (red + swir2)},
        SR      = function(red, nir) {nir / red},     
        TVI     = function(red, nir) {sqrt((nir-red)/(nir+red)+0.5)},
        TTVI    = function(red, nir) {sqrt(abs((nir-red)/(nir+red) + 0.5))},
        WDVI    = function(red, nir) {nir - s * red}
)


BANDSdb <- lapply(.IDXdb, function(x) names(formals(x))) 

#' References and names to .IDXdb
#' @keywords internal
#' @noRd 
.IDX.REFdb <- list(
        DVI     = c("Richardson1977", "Difference Vegetation Index") ,
        CTVI    = c("Perry1984", "Corrected Transformed Vegetation Index"),
        EVI     = c("Huete1999", "Enhanced Vegetation Index"),
        EVI2    = c("Jiang 2008", "Two-band Enhanced Vegetation Index"), # Development of a two-band enhanced vegetation index without a blue band
        GEMI    = c("Pinty1992","Global Environmental Monitoring Index"),
        GNDVI    = c("Gitelson1998", "Green Normalised Difference Vegetation Index"),
        MSAVI    = c("Qi1994","Modified Soil Adjusted Vegetation Index"),
        MSAVI2    = c("Qi1994","Modified Soil Adjusted Vegetation Index 2"),
        MNDWI   = c("Xu2006", "Modified Normalised Difference Water Index"),       
        NBRI    = c("Garcia1991", "Normalised Burn Ratio Index"),
        NDVI    = c("Rouse1974", "Normalised Difference Vegetation Index"),
        NDVIC   = c("Nemani1993", "Corrected Normalised Difference Vegetation Index"),
        NDWI    = c("McFeeters1996", "Normalised Difference Water Index"), # The use of the Normalized Difference Water Index (NDWI) in the delineation of open water features
        NDWI2    = c("Gao1996", "Normalised Difference Water Index"),
        NRVI    = c("Baret1991","Normalised Ratio Vegetation Index"),
        RVI     = c("", "Ratio Vegetation Index"),
        SATVI   = c("Marsett2006", "Soil Adjusted Total Vegetation Index"),
        SAVI    = c("Huete1988", "Soil Adjusted Vegetation Index"),
        SLAVI    = c("Lymburger2000","Specific Leaf Area Vegetation Index"),
        SR      = c("Birth1968", "Simple Ratio Vegetation Index"),  #or Jordan1969
        TVI     = c("Deering1975","Transformed Vegetation Index"),
        TTVI    = c("Thiam1997", "Thiam's Transformed Vegetation Index"),
        WDVI    = c("Richardson1977","Weighted Difference Vegetation Index")
)



.wavlDB <- data.frame( Band = c("vis", "nir", "swir1", "swir2", "swir3", "mir1", "mir2", "tir1", "tir2"), 
        Description = c("visible", "near infra-red", "short-wave infra-red", "short-wave infra-red", "short-wave infra-red", "mid-wave infra-red", "mid-wave infra-red", "thermal infra-red", "thermal infra-red"),
        Wavl_min = c(400,700,1100,1400,2000,3000,45000,8000,10000), 
        Wavl_max = c(700,1100,1351, 1800,2500,4000,5000,9500,140000),
        "Landsat5_Band" = c("1,2,3", 4, "-", 5, 7, "-", "-", "-", 6)
) 




