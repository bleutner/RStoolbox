#' Spectral Indices
#' 
#' Calculate a suite of multispectral indices such as NDVI, SAVI etc. in an efficient way.
#' 
#' @param img SpatRaster. Typically remote sensing imagery, which is to be classified.
#' @param blue Character or integer. Blue band. 
#' @param green Character or integer. Green band. 
#' @param red Character or integer. Red band. 
#' @param nir Character or integer. Near-infrared band (700-1100nm).
#' @param redEdge1 Character or integer. Red-edge band (705nm)
#' @param redEdge2 Character or integer. Red-edge band (740nm)
#' @param redEdge3 Character or integer. Red-edge band (783nm)
#' @param swir1 not used
#' @param swir2 Character or integer. Short-wave-infrared band (1400-1800nm). 
#' @param swir3 Character or integer. Short-wave-infrared band (2000-2500nm). 
#' @param indices Character. One or more spectral indices to calculate (see Details). By default (NULL) all implemented indices given the spectral bands which are provided will be calculated.
#' @param index Character. Alias for \code{indices}.
#' @param maskLayer RasterLayer or SpatRaster containing a mask, e.g. clouds, for which pixels are set to NA. Alternatively a layername or -number can be provided if the mask is part of \code{img}.
#' @param maskValue Integer. Pixel value in \code{maskLayer} which should be masked in output, i.e. will be set to \code{NA} in all calculated indices.
#' @param scaleFactor Numeric. Scale factor for the conversion of scaled reflectances to [0,1] value range (applied as reflectance/scaleFactor) Neccesary for calculating EVI/EVI2 with scaled reflectance values.
#' @param skipRefCheck Logical. When EVI/EVI2 is to be calculated there is a rough heuristic check, whether the data are inside [0,1]+/-0.5 (after applying a potential \code{scaleFactor}).
#'  If there are invalid reflectances, e.g. clouds with reflectance > 1 this check will result in a false positive and skip EVI calculation. Use this argument to skip this check in such cases *iff* you are sure the data and scaleFactor are valid. 
#' @param coefs List of coefficients (see Details).  
#' @param ... further arguments such as filename etc. passed to \link[terra]{writeRaster}
#' @return  SpatRaster
#' @template spectralIndices_table 
#' @export
#' @examples
#' library(ggplot2)
#' library(terra)
#' 
#' ## Calculate NDVI
#' ndvi <- spectralIndices(lsat, red = "B3_dn", nir = "B4_dn", indices = "NDVI")
#' ndvi
#' ggR(ndvi, geom_raster = TRUE) +
#'         scale_fill_gradientn(colours = c("black", "white")) 
#'
#' \donttest{ 
#' ## Calculate all possible indices, given the provided bands 
#' ## Convert DNs to reflectance (required to calculate EVI and EVI2)
#' mtlFile  <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")
#' lsat_ref <- radCor(lsat, mtlFile, method = "apref")
#' 
#' SI <- spectralIndices(lsat_ref, red = "B3_tre", nir = "B4_tre")
#' plot(SI)
#' }
spectralIndices <- function(img,
        blue=NULL, green=NULL, red=NULL, nir = NULL, 
        redEdge1 = NULL, redEdge2 = NULL, redEdge3 = NULL, 
        swir1 = NULL, swir2 =NULL, swir3 = NULL,
        scaleFactor = 1, skipRefCheck = FALSE,   indices=NULL, index = NULL, maskLayer = NULL, maskValue = 1,
        coefs = list(L = 0.5,  G = 2.5, L_evi = 1,  C1 = 6,  C2 = 7.5, s = 1, swir2ccc = NULL, swir2coc = NULL),
        ... ) {
    # TODO: soil line estimator
    
    ## We will use the following wavlength range definitions (following Schowengerdt 2007, p 10)
    # VIS   | Visible              |   400  -    700 nm
    # BLUE   |
    # GREEN |
    # RED  |
    # redEdge1
    # redEdge2
    # redEdge3
    # NIR   | Near infra-red       |   700  -   1100 nm
    # swir1 | Short-wave infra-red |  1100  -   1351 nm
    # swir2 | shortwave infra-red  |  1400  -   1800 nm
    # swir3 | Shortwave infra-red  |  2000  -   2500 nm
    # mir1  | midwave infra-red    |  3000  -   4000 nm 
    # mir2  | midwave infra-red    |  4500  -   5000 nm 
    # TIR1  | thermal infra-red    |  8000  -   9500 nm
    # TIR2  | thermal infra-red    | 10000  - 140000 nm
    ##    
	img <- .toTerra(img)
	if(!is.null(maskLayer)) maskLayer <- .toTerra(maskLayer)
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
    ind <- if(is.null(indices)) names(BANDSdb) else indices <- toupper(indices)  
    if((is.null(coefs$swir2ccc) | is.null(coefs$swir2coc))) {
        if(!is.null(indices) & ("NDVIC" %in% ind)) warning("NDVIc can only be calculated if swir2ccc and swir2coc coefficients are provided.", call. = FALSE)  
        coefs$swir2ccc <- 0  ## dummy, cant pass NULL to spectralIndicesCpp
        coefs$swir2coc <- 1  ## dummy, cant pass NULL to spectralIndicesCpp
        ind <- setdiff(ind, "NDVIC")
    }
    if(!any(ind %in% names(BANDSdb))) stop("indices must either be NULL to calculate all indices",
                "\nor element of c(", paste0(names(BANDSdb),collapse=","),") for specific indices.", call. = FALSE)
    coefs$swir2cdiff <- coefs$swir2coc - coefs$swir2ccc 
    if(coefs$swir2cdiff <= 0) stop("NDVIc coefficient swir2ccc (completeley closed canopy) must be smaller than swir2coc (completely open canopy)", call. = FALSE)
    
    ## Gather function arguments (all provided bands) and create args
    potArgs  <- c("blue", "green", "red", "redEdge1", "redEdge2", "redEdge3", "nir", "swir1", "swir2", "swir3")
    actArgs  <- vapply(potArgs, function(x) !is.null(get(x)), logical(1))    
    bands    <- potArgs[actArgs]
    
    ## Subset calculated indices to possible based on band inputs and / or user request
    requested    <- BANDSdb[ind]
    
    canCalc  <- names(requested)[!vapply(requested, function(x) any(!x %in% bands), logical(1))]
    if(!skipRefCheck && any(c("EVI","EVI2") %in% canCalc)){
        img[[red]] <- setMinMax(img[[red]])
        mm <- minmax(img[[red]])

        if(is.nan(mm[2,1]) || is.nan(mm[1,1]) || mm[2,1]/scaleFactor > 1.5 || mm[1,1]/scaleFactor < -0.5){
            ## checking for range [0,1] +/- .5 to allow for artifacts in reflectance.
            warning("EVI/EVI2 parameters L_evi, G, C1 and C2 are defined for reflectance [0,1] but img values are outside of this range.\n",
                    "  If you are using scaled reflectance values please provide the scaleFactor argument.\n", 
                    "  If img is in DN or radiance it must be converted to reflectance.\n",
                    "  Skipping EVI calculation.\n", call. = FALSE)
            canCalc <- canCalc[!canCalc %in% c("EVI", "EVI2")]
            indices <- indices[!indices %in% c("EVI", "EVI2")]
        }
    }
    
    ind  <- ind[ind %in% canCalc]   
    
    if(!length(ind)) stop("No index could be calculated. At least for one index you must specify *all* required bands.",
                "\n  See ?spectralIndices for information on required bands per index.", call. = FALSE)
    if(length(ind) < length(indices)){
        not <- setdiff(indices,ind)
        notbands <- setdiff(unlist(requested[not]), bands)
        warning("The following indices were requested but cannot be calculated: ", paste(not, collapse = ", "),
                "\n  because the following required bands were not specified: ",
                paste(notbands, collapse = ", "),
                "\n  The remaining fully specified indices will be calculated.", call. = FALSE)
    }
    
    ## Get required designated bands
    retrieve   <- lapply(bands, get, envir=environment())
    bandsCalc  <- vapply(retrieve, function(xi) {  if(is.character(xi)) match(xi, names(img)) else xi  }, numeric(1))
    names(bandsCalc) <- bands       
    
    ## Add mask layer to img if present
    if(!is.null(maskLayer)){
        if(inherits(maskLayer, "SpatRaster")) {
            if(nlyr(maskLayer) > 1) warning(sprintf("maskLayer has %s layers. Only the first layer will be used for masking.", nlyr(maskLayer)),call.=FALSE)
            img <- c(img, maskLayer[[1]])
            names(img)[nlyr(img)] <- "mask"
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
    
    wopt <- list(...)
    filename <- if("filename" %in% names(wopt)) wopt$filename else ""
    overwrite <- if("overwrite" %in% names(wopt)) wopt$overwrite else FALSE
    wopt[c("filename","overwrite")] <- NULL
    
    
    # Perform calculations 
    indexMagic <- terra::app(img[[bandsCalc]], fun = function(m) {
                        
                        spectralIndicesCpp(
                                x = m,
                                indices   = canCalc,
                                blueBand  = fullSet[["blue"]],
                                greenBand = fullSet[["green"]], 
                                redBand   = fullSet[["red"]],
                                redEdge1Band =  fullSet[["redEdge1"]],
                                redEdge2Band =  fullSet[["redEdge2"]],
                                redEdge3Band =  fullSet[["redEdge3"]],
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
                     wopt=wopt, filename = filename, overwrite = overwrite) 
    
    names(indexMagic) <- canCalc      
    
    return(indexMagic)
}

## NOT USED FOR CALCULATIONS ONLY FOR DOCUMENTATION
## SEE /src/spectraIndices.cpp for calculations
#' Database of spectral indices
#' @keywords internal
#' @noRd 
.IDXdb <- list(
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
                function(red, nir) {nir - s * red})
)


BANDSdb <- lapply(lapply(.IDXdb,"[[", 2), function(x) names(formals(x))) 
.IDXdbFormulae <- lapply(.IDXdb,"[[",2)
.IDX.REFdb <- lapply(.IDXdb,"[[",1)