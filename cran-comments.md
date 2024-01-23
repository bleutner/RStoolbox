This release fixes test results as requested by CRAN(Ripley/10.2.2022).
Plus additional functional changes and fixes.

### R CMD checks
### Test environments
* Ubuntu 20.04 64bit (oldrel, release, devel)
* winbuilder (oldrel, release, devel)
* MacOS (release)

### R CMD check results
There were no ERRORs or WARNINGs

### Downstream dependencies
fieldRS OK
foster OK
rtsVis OK
PlanetNICFI OK
spatialEco OK

### Changelog:
#### RStoolbox 0.3.0
###### New:
* `rasterCVA()` by default no longer enforces a minimal change magnitude (can still be accomplished with the `tmf` argument).
   Also a new argument `nct` allows to fix this threshold to a user selected value instead of deriving it based on the median of the observed change magnitudes. 
* `unsuperClass()` has a new argument `output` which allows to return the distances to all cluster centers as raster layers, instead of the class itself 
* added spectral index kNDVI in `spectralIndices()` as suggested by Camps-Valls et al (2021)
* added support for `terra::SpatRast` objects throughout RStoolbox (as alternative to `raster` objects). Note: internal functionality is still based on `raster`.

##### Changes:
* arguments `master` and `slave` in `coregisterImages()` were deprecated in favor of `ref` and `img`, respectively (closes #63, suggested by @MatthiasSiewert)

##### Fixes:
* `rasterCVA()` estimates median values now for entire rasters and not per chunk
* `cloudMask()` now returns NA for non-clouds instead of NaN
* `topCor()` now works for tiny rasters as well (fixes #55, reported by @latenooker) 
* `rasterPCA()` now correctly considers the number observations in face of missing values (fixes #79, reported by @andliszmmu)
* `superClass()` now accepts different geometries for trainData and valData (fixes #73, suggested by @Silviculturalist)
* fix `readMeta()` for MTL files delivered with Landsat collection data (fixes #71, reported by @jkoellin et al.)
