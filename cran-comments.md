
## CRAN checks
This submission contains a series of bug fixes

## Test environments
* ubuntu 15.04 64bit, (R 3.2.2, devel)
* ubuntu 12.04 (on travis-ci), (oldrel, release, devel)
* win-builder (release, devel)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## Downstream dependencies
none

## Changelog RStoolbox 0.1.3
* new logical argument `predict` for superClass. Disables prediction of full raster (validation is still conducted).
* new generic predict() function for superClass objects. Useful to separate model training and prediction. 

Fixes:
* fix histMatch for single layers (affected also 'ihs' pan-sharpening)

 