
## CRAN checks
This submission fixes the clang-UBSAN errors found in RStoolbox 0.1.1.

## Test environments
* ubuntu 15.04 64bit, (R 3.2.2, devel)
* ubuntu 12.04 (on travis-ci), (oldrel, release, devel)
* win-builder (release, devel)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## Downstream dependencies
none

## Changelog RStoolbox 0.1.2
New model for superClass: maximum likelihood classification (model = "mlc")
Fixes:
* Restrict calculation of EVI/EVI2 to reflectance data (#3)
* Enforce valid value ranges in radCor: radiance: [0,+Inf], reflectance: [0,1]. Includes a new argument `clamp` to turn this on or off (on by default).

 