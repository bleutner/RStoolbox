REVISION: Cut-down on example execution times which failed to pass CRAN submission pre-check.

NOTE: The maintainer email address has been changed from benjamin.leutner@uni-wuerzburg.de to rstoolboxpackage@gmail.com which is futureproof. As requested by CRAN policy, I have sent a confirmation email for this change from the previous email address.

This release restores compatibility with the raster package (Email from CRAN/Hornik, 11.07.19).
Plus other minor fixes and new functionality (see Changelog below).


### R CMD checks
### Test environments
* Ubuntu 19.04 64bit (release)
* Travis-CI Ubuntu 14.04 (devel)
* winbuilder (devel, release, oldrel)
* macos r-hub

### R CMD check results
There is one NOTE: change in maintainer email address
There were no ERRORs, WARNINGs

### Downstream dependencies
moveVis OK

### Changelog:
RStoolbox 0.2.6
New:
* added several Sentinel-2 optimized spectral indices relying on red-edge bands: 
   - red-edge inflection point (REIP),
   - normalized difference red-edge indices (NDREI1, NDREI2),
   - green-band chlorophyll index (CLG), red-edge chlorophyll index (CLRE)
   - Modified Chlorophyll Absorption Ratio Index (MCARI) 
   - MERIS Terrestrial Chlorophyll Index (MTCI)

Fixes: 
* `readSLI` and `writeSLI` now handle endian of binary spectral libraries correctly (#47, fix contributed by @aloboa)
* fix calculation of prediction probabilities in `superClass`(reported by Benson Kemboi)
* adapt to raster 2.9.5 API changes
* fix order of thermal calibration coefficients for Landsat 8 L1 MTL metadata in `readMeta` (reported by Xiaoma Li)
* fixed an issue where `readSLI` did not find header files with dots in pathnames (#51, reported by @aloboa)

Changes:
* modified readSLI label parsing. Internal white space is now converted to underscores (#52)

