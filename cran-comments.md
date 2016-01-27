## CRAN checks
## Test environments
* Ubuntu 15.04 64bit (release)
* Docker/Debian 8.1 (R 3.2.1 with clang, UBSAN) 
* Ubuntu 12.04 (on travis-ci), (oldrel, release, devel)
* Win-builder (release, devel)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## Downstream dependencies
none



## Changelog RStoolbox 0.1.4
New:
* new function `validateMap()` for assessing map accuracy separately from model fitting, e.g. after majority or MMU filtering
* new function `getValidation()` to extract specific validation results of superClass objects (proposed by James Duffy)
* new spectral index NDVIc (proposed by Jeff Evans)
* new argument scaleFactor for `spectralIndices()` for calculation of EVI/EVI2 based on scaled reflectance values. 
* implemented dark object subtraction radCor(..,method='sdos') for Landsat 8 data (@BayAludra, #4)

Changes:
* superClass based on polygons now considers only pixels which have their center coordinate within a polygon  
* rasterCVA now returns angles from 0 to 360° instead of 0:45 by quadrant (reported by Martin Wegmann)
* improved dark object DN estimation based on maximum slope of the histogram in `estimateHaze` (@BayAludra, #4)

Fixes:
* superClass failed when neither valData or trainPartition was specified. regression introduced in 0.1.3 (reported by Anna Stephani)
* spectralIndices valid value range of EVI/EVI2 now [-1,1]
* radCor returned smallest integer instead of NA for some NA pixels
* fix 'sdos' for non-contiguous bands in radCor (@BayAludra, #4)


 