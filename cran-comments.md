## CRAN checks
## Test environments
* Ubuntu 16.04 64bit (release)
* Docker/Debian 8.1 (R 3.3.1 with clang and UBSAN) 
* Ubuntu 12.04 (on travis-ci), (oldrel, release, devel)
* Win-builder (release, devel)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:
Possibly mis-spelled words in DESCRIPTION:
  indices (11:26)

This is a false alarm; 'indices' is spelled correctly.  


## Downstream dependencies
none



## Changelog RStoolbox 0.1.5
Changes:
* If the bandSet argument in `radCor()` is used to process only a subset of bands it will no longer return unprocessed bands along with processed bands. Instead only processed bands are returned.
* By default `superClass()` will now use `dataType = 'INT2S'` for classification maps to avoid issues with raster NA handling in INT1U
* Allow reading and importing from Landsat MSS MTL files with `readMeta()` and `stackMeta()` (@aszeitz, #7)

Fixes:
* `readMeta()` time-stamp conversion now correctly set to GMT time (@mraraju, #12)
* `radCor()` caused R to crash if bandSet was a single band 
* fix single RasterLayer capability for `superClass()`
* `spectralIndices()` now calculates *all* documented indices if specified to do so (@mej1d1, #6)
* `unsuperClass()` predicted map now handles NAs properly
* `pifMatch()` did not return adjusted image (@tmb3006, #13)

Deprecated:
* argument `norm` was dropped from `rasterPCA()`, because it was effectively a duplicate of the standardized pca (spca) argument in the same function.

 