# RStoolbox 1.0.2
Changes for publication in MEE (2)

## Changes:
* `spectralIndices()` rolled back to a minimal version of the customizing option for the `spectralIndices` function. Supported functionalities are addition, subtraction, multiplication, division, and parentheses. This is due warnings for the Mac M1 on CRAN. We will implement more functionalities soon again as, e.g., `abs()`, `exp()`, ...

# RStoolbox 1.0.1
Changes for publication in MEE

## New:
* `spectralIndices()` can now be customized. We made the indices available as an editable option in the RStoolbox package as suggested by a reviewer. Users now can append a custom index to it that will be then calculated via the C++ plus within spectralIndices.cpp.

## Changes:
* `mesma()` now does not throw an error anymore when given no models and the default value was too high

# RStoolbox 1.0.0
Minor changes before major release

## New:
* `mesma()` now better differentiates SMA and MESMA: For single endmember unmixing, each supplied endmember represents a class to unmix (row by row). For multiple endmemeber unmixing, the column `class` can be used to group endmembers by class. If multiple endmembers per class are provided, `mesma()` will compute a number of SMA (determined through the new argument `n_models`) for multiple endmember combinations drawn from endmembers and will select the best fit per pixel based on the lowest RMSE. See `?mesma` for details (fixes #57, reported by @ytarazona)

## Changes:
* `mesma()` now implements the sum to one constraint by default (argument `sum_to_one`) (fixes #62, reported by @michaeldorman)
* added a new example to `mesma()` to reflect the changes

# RStoolbox 0.4.0
Rewrite of `RStoolbox`, migration from `raster` to `terra` and `sp` to `sf`

## New:
* RStoolbox moved on from the outdated `sp` and `raster` packages to `sf` and `terra` to ensure long term support of the tools.
* Thrown out unnecessary libraries

## Fixes:
* `rasterPCA()`: Fixed a bug that caused the method and its unit tests to fail on Linux due to a corrupted covariance matrix calculated previously with `terra::layerCor()`
* `superClass()` unable to predict when there is NA in raster data (closes #102, reported by @bappa10085)

# RStoolbox 0.3.0

## New:
* `rasterCVA()` by default no longer enforces a minimal change magnitude (can still be accomplished with the `tmf` argument).
   Also a new argument `nct` allows to fix this threshold to a user selected value instead of deriving it based on the median of the observed change magnitudes. 
* `unsuperClass()` has a new argument `output` which allows to return the distances to all cluster centers as raster layers, instead of the class itself 
* added spectral index kNDVI in `spectralIndices()` as suggested by Camps-Valls et al (2021)
* added support for `terra::SpatRast` objects throughout RStoolbox (as alternative to `raster` objects). Note: internal functionality is still based on `raster`.

## Changes:
* arguments `master` and `slave` in `coregisterImages()` were deprecated in favor of `ref` and `img`, respectively (closes #63, suggested by @MatthiasSiewert)

## Fixes:
* `rasterCVA()` estimates median values now for entire rasters and not per chunk
* `cloudMask()` now returns NA for non-clouds instead of NaN
* `topCor()` now works for tiny rasters as well (fixes #55, reported by @latenooker) 
* `rasterPCA()` now correctly considers the number observations in face of missing values (fixes #79, reported by @andliszmmu)
* `superClass()` now accepts different geometries for trainData and valData (fixes #73, suggested by @Silviculturalist)
* fix `readMeta()` for MTL files delivered with Landsat collection data (fixes #71, reported by @jkoellin et al.)

# RStoolbox 0.2.6

## New:
* added several Sentinel-2 optimized spectral indices relying on red-edge bands: 
   - red-edge inflection point (REIP),
   - normalized difference red-edge indices (NDREI1, NDREI2),
   - green-band chlorophyll index (CLG), red-edge chlorophyll index (CLRE)
   - Modified Chlorophyll Absorption Ratio Index (MCARI) 
   - MERIS Terrestrial Chlorophyll Index (MTCI)

## Fixes: 
* `readSLI()` and `writeSLI()` now handle endian of binary spectral libraries correctly (#47, fix contributed by @aloboa)
* fix calculation of prediction probabilities in `superClass()`(reported by Benson Kemboi)
* adapt to raster 2.9.5 API changes
* fix order of thermal calibration coefficients for Landsat 8 L1 MTL metadata in `readMeta` (reported by Xiaoma Li)
* fixed an issue where `readSLI()` did not find header files with dots in pathnames (#51, reported by @aloboa)

## Changes:
* modified readSLI label parsing. Internal white space is now converted to underscores (#52)


# RStoolbox 0.2.4

## New:

* function `oneHotEncode()`: splits a single rasterLayer into multiple layers (one per class)
  with one-hot encoding, i.e. pixel value = 1 for matches, pixel value = 0 for all other classes (background).
* `ggR()` can now display more than one layer. Each layer can be plotted to a subplot in a multi-panel figure.
* `encodeQA()`, `decodeQA()` and `classifyQA()` can now deal with the new QA format introduced with Landsat Collection data. Legacy QA designations can still be interpreted by setting the `legacy` argument.
* new `predict()` method for unsupervised classification models (`unsuperClass()`).

## Changes: 
* all `radCor()` DOS methods now work for Landsat 8 OLI

## Fixes:
* fix `unsuperClass()` for algorithms other than Hartigan-Wong (reported by Alex Ilich)

# RStoolbox 0.2.2

## New:
* added `tasseledCap()` coefficients for Quickbird, Spot5 and RapidEye

## Changes:
* `readEE()` 'Date' column is now returned as POSIXct instead of POSIXlt

## Fixes:
* corrected `tasseledCap()` coefficient for Landsat 5 TM (#40, reported by @philipperufin)


# RStoolbox 0.2.1

## Fixes:
* fixed non-portable c++ headers

# RStoolbox 0.2.0

## New:
* function `mesma()` for spectral unmixing (#33, provided by Jakob Schwalb-Willmann)

## Fixes: 
* improved NA handling and faster implementation of mlc classifier (#32, pull request by Neal Fultz)
* adapt to upcoming caret version (new constraint caret >= 6.0-79)


# RStoolbox 0.1.10

## Fixes:
* fix tests for upcoming raster version

# RStoolbox 0.1.9

## Fixes:
* adapt to new caret version
* fix `readEE()` for new EarthExplorer formats
* corrected sign of greenness `tasseledCap()` coefficient for Landsat5 TM band 1 (reported by Thomas Day)
* adapt `readMeta()` and `stackMeta()` to new Landsat collection 1 metadata

# RStoolbox 0.1.8

## New:
* `spectralIndices()` can now apply a mask internally, e.g. to exclude cloud pixels. New arguments are: 
   `maskLayer` and `maskValue` (suggested by Andrea Hess).   
* added spectral index GNDWI   

## Fixes: 
* update `readEE()` to deal with new EarthExplorer export columns (reported by Christian Bauer)

# RStoolbox 0.1.7

## New:
* `spectralIndices()` has a new argument skipRefCheck, which skips the heuristic check for reflectance-like values [0,1] which is run if EVI/EVI2 are requested.
   This can be usefull if clouds with reflectance > 1.5 are part of the image.
* `superClass()` now returns the geometries which were used for validation, e.g. polygons (under $validation$geometry)
   and also the exact samples taken for validation including cell number and coordinates ($validation$validationSamples)
*  added example data-set for spectral library see ?readSLI
*  increased overall test coverage 

## Changes:
* ESUN lookup tables for `radCor()` are adjusted to match current USGS recommendations from: https://landsat.usgs.gov/esun
* `spectralIndices()` swir wavelength ranges are now defined consistently and correctly. 
   Bands formerly provided as swir1 (version <1.7.0) should now (>=1.7.0) be provided as swir2 and former swir2 as swir3 respectively (see docu). 
   The actual calculations were correct, but the naming was off. 
   
## Fixes:
* fix `ggR()` and `ggRGB()` in  annotation mode (default). No image was drawn and excessive memory allocation requested (= RStudio crash) (reported by Christian Walther)
* fix `spectralIndices()` documentation for NDWI. Formula was based on McFeeters1996 but attributed to Gao1996. Now there is NDWI (McFeeters) and NDWI2 (Gao) (reported by Christian Bauer)
* `estimateHaze()` now ensures correct histogram order, which could be off when raster had to read from disk (reported by Xavier Bailleau).   
* `readMeta()` now makes concise bandnames also for Landsat Collection MTL files.
* fix `radCor()` for Landsat 4 TM (reported by Thomas Day)
* `classifyQA()` confidence layer for type='water' now correctly returns only confidence levels in [1,3] 
* enable reading ENVI plot files in ASCII mode with `readSLI()`

Deprecated:
* `spectralIndices()` index `LSWI` has been deprecated, as it is identical with the now available NDWI2.

# RStoolbox 0.1.6

## Fixes:
* fix import issue: replace deprecated export from caret 

# RStoolbox 0.1.5

## Changes:
* If the bandSet argument in `radCor()` is used to process only a subset of bands it will no longer return unprocessed bands along with processed bands. Instead only processed bands are returned.
* By default `superClass()` will now use dataType = 'INT2S' for classification maps to avoid issues with raster NA handling in INT1U
* Allow reading and importing from Landsat MSS MTL files with `readMeta()` and `stackMeta()` (@aszeitz, #7)

## Fixes:
* fix readMeta time-stamp conversion now correctly set to GMT time (@mraraju, #12)
* radCor caused R to crash if bandSet was a single band 
* fix single RasterLayer capability for superClass
* spectralIndices now calculates *all* documented indices if specified to do so (@mej1d1, #6)
* unsuperClass predicted map now handles NAs properly
* pifMatch did not return adjusted image (@tmb3006, #13)

Deprecated:
* argument `norm` was dropped from rasterPCA, because it was effectively a duplicate of the standardized pca (spca) argument in the same function.

# RStoolbox 0.1.4

## New:
* new function `validateMap()` for assessing map accuracy separately from model fitting, e.g. after majority or MMU filtering
* new function `getValidation()` to extract specific validation results of superClass objects (proposed by James Duffy)
* new spectral index NDVIc (proposed by Jeff Evans)
* new argument scaleFactor for `spectralIndices()` for calculation of EVI/EVI2 based on scaled reflectance values. 
* implemented dark object subtraction radCor(..,method='sdos') for Landsat 8 data (@BayAludra, #4)

## Changes:
* superClass based on polygons now considers only pixels which have their center coordinate within a polygon  
* rasterCVA now returns angles from 0 to 360° instead of 0:45 by quadrant (reported by Martin Wegmann)
* improved dark object DN estimation based on maximum slope of the histogram in `estimateHaze()` (@BayAludra, #4)

## Fixes:
* superClass failed when neither valData or trainPartition was specified. regression introduced in 0.1.3 (reported by Anna Stephani)
* spectralIndices valid value range of EVI/EVI2 now [-1,1]
* radCor returned smallest integer instead of NA for some NA pixels
* fix 'sdos' for non-contiguous bands in radCor (@BayAludra, #4)


# RStoolbox 0.1.3

## New:
* new logical argument `predict()` for superClass. Disables prediction of full raster (validation is still conducted).
* new generic predict() function for superClass objects. Useful to separate model training and prediction. 
* new example data set (landcover training polygons) for lsat example data under /extdata/trainingPolygons.rds 

## Fixes:
* fix histMatch for single layers (affected also 'ihs' pan-sharpening)
* fix superClass validation sampling for factors (character based factors could lead to wrong factor conversions and wrong validation results)
* improved handling of of training polygons with overlaps and shared borders in superClass
* improved checks and error messages for insufficient training polygons

# RStoolbox 0.1.2

## New:
New model for superClass: maximum likelihood classification (model = "mlc")

## Fixes:
* Restrict calculation of EVI/EVI2 to reflectance data (#3)
* Enforce valid value ranges in `radCor()`: radiance: [0,+Inf], reflectance: [0,1]. Includes a new argument `clamp` to turn this on or off (on by default).

# RStoolbox 0.1.1

Added kernlab to suggested packages to be able to test \donttest{} examples

# RStoolbox 0.1.0

Initial release to CRAN (2015-09-05) with the following functions: 
 * classifyQA()
 * cloudMask()
 * cloudShadowMask()
 * coregisterImages()
 * decodeQA()
 * encodeQA()
 * estimateHaze()
 * fortify.raster()
 * fCover()
 * getMeta()
 * ggR()
 * ggRGB()
 * histMatch()
 * ImageMetaData()
 * normImage()
 * panSharpen()
 * pifMatch()
 * radCor()
 * rasterCVA()
 * rasterEntropy()
 * rasterPCA()
 * readEE()
 * readMeta()
 * readRSTBX()
 * readSLI()
 * rescaleImage()
 * rsOpts()
 * sam()
 * saveRSTBX()
 * spectralIndices()
 * stackMeta()
 * superClass()
 * tasseledCap()
 * topCor()
 * unsuperClass()
 * writeSLI() 
 
Included example data sets: 
* data(srtm)
* data(lsat)
* data(rlogo)


