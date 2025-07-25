% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spectralIndices.R
\name{spectralIndices}
\alias{spectralIndices}
\title{Spectral Indices}
\usage{
spectralIndices(
  img,
  blue = NULL,
  green = NULL,
  red = NULL,
  nir = NULL,
  redEdge1 = NULL,
  redEdge2 = NULL,
  redEdge3 = NULL,
  swir1 = NULL,
  swir2 = NULL,
  swir3 = NULL,
  scaleFactor = 1,
  skipRefCheck = FALSE,
  indices = NULL,
  index = NULL,
  maskLayer = NULL,
  maskValue = 1,
  coefs = list(L = 0.5, G = 2.5, L_evi = 1, C1 = 6, C2 = 7.5, s = 1, swir2ccc = NULL,
    swir2coc = NULL),
  ...
)
}
\arguments{
\item{img}{SpatRaster. Typically remote sensing imagery, which is to be classified.}

\item{blue}{Character or integer. Blue band.}

\item{green}{Character or integer. Green band.}

\item{red}{Character or integer. Red band.}

\item{nir}{Character or integer. Near-infrared band (700-1100nm).}

\item{redEdge1}{Character or integer. Red-edge band (705nm)}

\item{redEdge2}{Character or integer. Red-edge band (740nm)}

\item{redEdge3}{Character or integer. Red-edge band (783nm)}

\item{swir1}{not used}

\item{swir2}{Character or integer. Short-wave-infrared band (1400-1800nm).}

\item{swir3}{Character or integer. Short-wave-infrared band (2000-2500nm).}

\item{scaleFactor}{Numeric. Scale factor for the conversion of scaled reflectances to [0,1] value range (applied as reflectance/scaleFactor) Neccesary for calculating EVI/EVI2 with scaled reflectance values.}

\item{skipRefCheck}{Logical. When EVI/EVI2 is to be calculated there is a rough heuristic check, whether the data are inside [0,1]+/-0.5 (after applying a potential \code{scaleFactor}).
If there are invalid reflectances, e.g. clouds with reflectance > 1 this check will result in a false positive and skip EVI calculation. Use this argument to skip this check in such cases *iff* you are sure the data and scaleFactor are valid.}

\item{indices}{Character. One or more spectral indices to calculate (see Details). By default (NULL) all implemented indices given the spectral bands which are provided will be calculated.}

\item{index}{Character. Alias for \code{indices}.}

\item{maskLayer}{RasterLayer or SpatRaster containing a mask, e.g. clouds, for which pixels are set to NA. Alternatively a layername or -number can be provided if the mask is part of \code{img}.}

\item{maskValue}{Integer. Pixel value in \code{maskLayer} which should be masked in output, i.e. will be set to \code{NA} in all calculated indices.}

\item{coefs}{List of coefficients (see Details).}

\item{...}{further arguments such as filename etc. passed to \link[terra]{writeRaster}}
}
\value{
SpatRaster
}
\description{
Calculate a suite of multispectral indices such as NDVI, SAVI etc. in an efficient way via C++.
}
\details{
\code{spectralIndices} calculates all indices in one go in C++,  which is more efficient than calculating each index separately (for large rasters).
By default all indices which can be calculated given the specified indices will be calculated. If you don't want all indices, use the \code{indices} argument to specify exactly which indices are to be calculated.
See the table bellow for index names and required bands.
   
Index values outside the valid value ranges (if such a range exists) will be set to NA. For example a pixel with NDVI > 1 will be set to NA.

 





\tabular{lllll}{ \strong{ Index } \tab \strong{ Description } \tab \strong{ Source } \tab \strong{ Bands } \tab \strong{ Formula } \cr CLG \tab Green-band Chlorophyll Index \tab Gitelson2003 \tab \code{redEdge3, green} \tab \eqn{redEdge3/green - 1} \cr CLRE \tab Red-edge-band Chlorophyll Index \tab Gitelson2003 \tab \code{redEdge3, redEdge1} \tab \eqn{redEdge3/redEdge1 - 1} \cr CTVI \tab Corrected Transformed Vegetation Index \tab Perry1984 \tab \code{red, nir} \tab \eqn{(NDVI + 0.5)/sqrt(abs(NDVI + 0.5))} \cr DVI \tab Difference Vegetation Index \tab Richardson1977 \tab \code{red, nir} \tab \eqn{s * nir - red} \cr EVI \tab Enhanced Vegetation Index \tab Huete1999 \tab \code{red, nir, blue} \tab \eqn{G * ((nir - red)/(nir + C1 * red - C2 * blue + L_evi))} \cr EVI2 \tab Two-band Enhanced Vegetation Index \tab Jiang 2008 \tab \code{red, nir} \tab \eqn{G * (nir - red)/(nir + 2.4 * red + 1)} \cr GEMI \tab Global Environmental Monitoring Index \tab Pinty1992 \tab \code{red, nir} \tab \eqn{(((nir^2 - red^2) * 2 + (nir * 1.5) + (red * 0.5))/(nir + red + 0.5)) * (1 - ((((nir^2 - red^2) * 2 + (nir * 1.5) + (red * 0.5))/(nir + red + 0.5)) * 0.25)) - ((red - 0.125)/(1 - red))} \cr GNDVI \tab Green Normalised Difference Vegetation Index \tab Gitelson1998 \tab \code{green, nir} \tab \eqn{(nir - green)/(nir + green)} \cr KNDVI \tab Kernel Normalised Difference Vegetation Index \tab Camps-Valls2021 \tab \code{red, nir} \tab \eqn{tanh(((nir - red)/(nir + red)))^2} \cr MCARI \tab Modified Chlorophyll Absorption Ratio Index \tab Daughtery2000 \tab \code{green, red, redEdge1} \tab \eqn{((redEdge1 - red) - (redEdge1 - green)) * (redEdge1/red)} \cr MNDWI \tab Modified Normalised Difference Water Index \tab Xu2006 \tab \code{green, swir2} \tab \eqn{(green - swir2)/(green + swir2)} \cr MSAVI \tab Modified Soil Adjusted Vegetation Index \tab Qi1994 \tab \code{red, nir} \tab \eqn{nir + 0.5 - (0.5 * sqrt((2 * nir + 1)^2 - 8 * (nir - (2 * red))))} \cr MSAVI2 \tab Modified Soil Adjusted Vegetation Index 2 \tab Qi1994 \tab \code{red, nir} \tab \eqn{(2 * (nir + 1) - sqrt((2 * nir + 1)^2 - 8 * (nir - red)))/2} \cr MTCI \tab MERIS Terrestrial Chlorophyll Index \tab DashAndCurran2004 \tab \code{red, redEdge1, redEdge2} \tab \eqn{(redEdge2 - redEdge1)/(redEdge1 - red)} \cr NBRI \tab Normalised Burn Ratio Index \tab Garcia1991 \tab \code{nir, swir3} \tab \eqn{(nir - swir3)/(nir + swir3)} \cr NDREI1 \tab Normalised Difference Red Edge Index 1 \tab GitelsonAndMerzlyak1994 \tab \code{redEdge2, redEdge1} \tab \eqn{(redEdge2 - redEdge1)/(redEdge2 + redEdge1)} \cr NDREI2 \tab Normalised Difference Red Edge Index 2 \tab Barnes2000 \tab \code{redEdge3, redEdge1} \tab \eqn{(redEdge3 - redEdge1)/(redEdge3 + redEdge1)} \cr NDVI \tab Normalised Difference Vegetation Index \tab Rouse1974 \tab \code{red, nir} \tab \eqn{(nir - red)/(nir + red)} \cr NDVIC \tab Corrected Normalised Difference Vegetation Index \tab Nemani1993 \tab \code{red, nir, swir2} \tab \eqn{(nir - red)/(nir + red) * (1 - ((swir2 - swir2ccc)/(swir2coc - swir2ccc)))} \cr NDWI \tab Normalised Difference Water Index \tab McFeeters1996 \tab \code{green, nir} \tab \eqn{(green - nir)/(green + nir)} \cr NDWI2 \tab Normalised Difference Water Index \tab Gao1996 \tab \code{nir, swir2} \tab \eqn{(nir - swir2)/(nir + swir2)} \cr NRVI \tab Normalised Ratio Vegetation Index \tab Baret1991 \tab \code{red, nir} \tab \eqn{(red/nir - 1)/(red/nir + 1)} \cr REIP \tab Red Edge Inflection Point \tab GuyotAndBarnet1988 \tab \code{red, redEdge1, redEdge2, redEdge3} \tab \eqn{0.705 + 0.35 * ((red + redEdge3)/(2 - redEdge1))/(redEdge2 - redEdge1)} \cr RVI \tab Ratio Vegetation Index \tab  \tab \code{red, nir} \tab \eqn{red/nir} \cr SATVI \tab Soil Adjusted Total Vegetation Index \tab Marsett2006 \tab \code{red, swir2, swir3} \tab \eqn{(swir2 - red)/(swir2 + red + L) * (1 + L) - (swir3/2)} \cr SAVI \tab Soil Adjusted Vegetation Index \tab Huete1988 \tab \code{red, nir} \tab \eqn{(nir - red) * (1 + L)/(nir + red + L)} \cr SLAVI \tab Specific Leaf Area Vegetation Index \tab Lymburger2000 \tab \code{red, nir, swir2} \tab \eqn{nir/(red + swir2)} \cr SR \tab Simple Ratio Vegetation Index \tab Birth1968 \tab \code{red, nir} \tab \eqn{nir/red} \cr TTVI \tab Thiam's Transformed Vegetation Index \tab Thiam1997 \tab \code{red, nir} \tab \eqn{sqrt(abs((nir - red)/(nir + red) + 0.5))} \cr TVI \tab Transformed Vegetation Index \tab Deering1975 \tab \code{red, nir} \tab \eqn{sqrt((nir - red)/(nir + red) + 0.5)} \cr WDVI \tab Weighted Difference Vegetation Index \tab Richardson1977 \tab \code{red, nir} \tab \eqn{nir - s * red} \cr CUSTOM \tab Super custom index \tab Mueller2024 \tab \code{red} \tab \eqn{red * 0} \cr CUSTOM2 \tab Super custom index \tab Mueller2024 \tab \code{swir1} \tab \eqn{swir1 - swir1} \cr CUSTOM \tab Super custom index \tab Mueller2024 \tab \code{red} \tab \eqn{red * 0} \cr CUSTOM2 \tab Super custom index \tab Mueller2024 \tab \code{swir1} \tab \eqn{swir1 - swir1} \cr CUSTOM \tab Super custom index \tab Mueller2024 \tab \code{red} \tab \eqn{red * 0} \cr CUSTOM2 \tab Super custom index \tab Mueller2024 \tab \code{swir1} \tab \eqn{swir1 - swir1} \cr CUSTOM \tab Super custom index \tab Mueller2024 \tab \code{red} \tab \eqn{red * 0} \cr CUSTOM2 \tab Super custom index \tab Mueller2024 \tab \code{swir1} \tab \eqn{swir1 - swir1} \cr CUSTOM \tab Super custom index \tab Mueller2024 \tab \code{red} \tab \eqn{red * 0} \cr CUSTOM2 \tab Super custom index \tab Mueller2024 \tab \code{swir1} \tab \eqn{swir1 - swir1} \cr CUSTOM \tab Super custom index \tab Mueller2024 \tab \code{red} \tab \eqn{red * 0} \cr CUSTOM2 \tab Super custom index \tab Mueller2024 \tab \code{swir1} \tab \eqn{swir1 - swir1} \cr CUSTOM \tab Super custom index \tab Mueller2024 \tab \code{red} \tab \eqn{red * 0} \cr CUSTOM2 \tab Super custom index \tab Mueller2024 \tab \code{swir1} \tab \eqn{swir1 - swir1} \cr CUSTOM \tab Super custom index \tab Mueller2024 \tab \code{red} \tab \eqn{red * 0} \cr CUSTOM2 \tab Super custom index \tab Mueller2024 \tab \code{swir1} \tab \eqn{swir1 - swir1} \cr CUSTOM \tab Super custom index \tab Mueller2024 \tab \code{red} \tab \eqn{red * 0} \cr CUSTOM2 \tab Super custom index \tab Mueller2024 \tab \code{swir1} \tab \eqn{swir1 - swir1} \cr CUSTOM \tab Super custom index \tab Mueller2024 \tab \code{red} \tab \eqn{red * 0} \cr CUSTOM2 \tab Super custom index \tab Mueller2024 \tab \code{swir1} \tab \eqn{swir1 - swir1} }


Some indices require additional parameters, such as the slope of the soil line which are specified via a list to the \code{coefs} argument. 
Although the defaults are sensible values, values like the soil brightnes factor \code{L} for SAVI should be adapted depending on the characteristics of the scene.
The coefficients are:
\tabular{lll}{
\strong{Coefficient} \tab \strong{Description} \tab \strong{Affected Indices} \cr
\code{s} \tab slope of the soil line \tab DVI, WDVI \cr
\code{L_evi, C1, C2, G} \tab various \tab EVI \cr
\code{L} \tab soil brightness factor \tab SAVI, SATVI \cr
\code{swir2ccc} \tab minimum swir2 value (completely closed forest canopy) \tab NDVIC \cr
\code{swir2coc} \tab maximum swir2 value (completely open canopy) \tab NDVIC \cr
}


The wavelength band names are defined following Schowengertd 2007, p10. 
The last column shows exemplarily which Landsat 5 TM bands correspond to which wavelength range definition.
\tabular{lllllll}{ \strong{ Band } \tab \strong{ Description } \tab \strong{ Wavl_min } \tab \strong{ Wavl_max } \tab \strong{ Landsat5_Band } \tab \strong{ Sentinel2_Band } \cr vis \tab visible \tab   400 \tab    680 \tab 1,2,3 \tab 2,3,4 \cr red-edge1 \tab red-edge1 \tab   680 \tab    720 \tab - \tab 5 \cr red-edge2 \tab red-edge2 \tab   720 \tab    760 \tab - \tab 6 \cr red-edge3 \tab red-edge3 \tab   760 \tab    800 \tab - \tab 7 \cr nir \tab near infra-red \tab   800 \tab   1100 \tab 4 \tab 8/8a \cr swir1 \tab short-wave infra-red \tab  1100 \tab   1351 \tab - \tab 9,10 \cr swir2 \tab short-wave infra-red \tab  1400 \tab   1800 \tab 5 \tab 11 \cr swir3 \tab short-wave infra-red \tab  2000 \tab   2500 \tab 7 \tab 12 \cr mir1 \tab mid-wave infra-red \tab  3000 \tab   4000 \tab - \tab - \cr mir2 \tab mid-wave infra-red \tab 45000 \tab   5000 \tab - \tab - \cr tir1 \tab thermal infra-red \tab  8000 \tab   9500 \tab - \tab - \cr tir2 \tab thermal infra-red \tab 10000 \tab 140000 \tab 6 \tab - }
}
\examples{
library(ggplot2)
library(terra)

## Calculate NDVI
ndvi <- spectralIndices(lsat, red = "B3_dn", nir = "B4_dn", indices = "NDVI")
ndvi
ggR(ndvi, geom_raster = TRUE) +
        scale_fill_gradientn(colours = c("black", "white")) 

\donttest{ 
## Calculate all possible indices, given the provided bands 
## Convert DNs to reflectance (required to calculate EVI and EVI2)
mtlFile  <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")
lsat_ref <- radCor(lsat, mtlFile, method = "apref")

SI <- spectralIndices(lsat_ref, red = "B3_tre", nir = "B4_tre")
plot(SI)

## Custom Spectral Index Calculation (beta) (supports only bands right now...)
# Get all indices
# Supports: Parentheses (), Addition +, Subtraction -, Multiplication *, Division /
idxdb <- getOption("RStoolbox.idxdb")

# Customize the RStoolbox index-database and overwrite the option
cdb <- c(idxdb, CUSTOM = list( list(c("Mueller2024", "Super custom index"),
        function(blue, red) {blue + red})))
rsOpts(idxdb = cdb)

# Calculate the custom index, (also together with the provided ones)
custom_ind <- spectralIndices(lsat, blue = 1, red = 3, nir = 4, indices = c("NDVI", "CUSTOM"))
}
}
