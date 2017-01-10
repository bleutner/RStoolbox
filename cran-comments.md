### R CMD checks
### Test environments
* Ubuntu 16.10 64bit (release)
* Ubuntu 12.04 (on travis-ci), (oldrel, release, devel)
* winbuilder (devel)
* r-hub: 'debian-gcc-release', 
    'debian-gcc-devel',
    'ubuntu-gcc-devel', 
    'windows-x86_64-oldrel', 
    'windows-x86_64-release',
    'windows-x86_64-devel',
    'linux-x86_64-rocker-gcc-san'  

### R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:
Possibly mis-spelled words in DESCRIPTION:
  indices (11:26)

This is a false alarm; 'indices' is spelled correctly.  

### Downstream dependencies
none

### Changelog RStoolbox 0.1.7
New:
* `spectralIndices()` has a new argument skipRefCheck, which skips the heuristic check for reflectance-like values [0,1] which is run if EVI/EVI2 are requested.
   This can be usefull if clouds with reflectance > 1.5 are part of the image.
* `superClass()` now returns the geometries which were used for validation, e.g. polygons (under $validation$geometry)
   and also the exact samples taken for validation including cell number and coordinates ($validation$validationSamples)
*  added example data-set for spectral library see ?readSLI
*  increased overall test coverage 

Changes:
* ESUN lookup tables for `radCor()` are adjusted to match current USGS reccomendations from: https://landsat.usgs.gov/esun
* `spectralIndices()` swir wavelength ranges are now defined consistently and correctly. 
   Bands formerly provided as swir1 (version <1.7.0) should now (>=1.7.0) be provided as swir2 and former swir2 as swir3 respectively (see docu). 
   The actual calculations were correct, but the naming was off. 
   
Fixes:
* fix `ggR()` and `ggRGB()` in  annotation mode (default). No image was drawn and excessive memory allocation requested (= RStudio crash) (reported by Christian Walther)
* fix `spectralIndices()` documentation for NDWI. Formula was based on McFeeters1996 but attributed to Gao1996. Now there is NDWI (McFeeters) and NDWI2 (Gao) (reported by Christian Bauer)
* `estimateHaze()` now ensures correct histogram order, which could be off when raster had to read from disk (reported by Xavier Bailleau).   
* `readMeta()` now makes concise bandnames also for Landsat Collection MTL files.
* fix `radCor()` for Landsat 4 TM (reported by Thomas Day)
* `classifyQA()` confidence layer for type='water' now correctly returns only confidence levels in [1,3] 
* enable reading ENVI plot files in ASCII mode with `readSLI()`

Deprecated:
* `spectralIndices()` index `LSWI` has been deprecated, as it is identical with the now available NDWI2.
