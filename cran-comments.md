Adapts tests to upcoming caret version. 
Should fix MacOS CRAN errors due to non-installed suggested package (rgdal).
Plus multiple smaller fixes.

### R CMD checks
### Test environments
* Ubuntu 16.10 64bit (release, release + valgrind)
* Ubuntu 12.04 (on travis-ci), (oldrel, release, devel)
* winbuilder (devel, release)

### R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:
Possibly mis-spelled words in DESCRIPTION:
  indices (10:26)

This is a false alarm; 'indices' is spelled correctly.  

### Downstream dependencies
none

### Changelog RStoolbox 0.1.9
Fixes:
* adapt to new caret version
* fix readEE for new EarthExplorer formats
* corrected sign of greenness tasseledCap coefficient for Landsat5 TM band 1 (reported by Thomas Day)
* adapt to new Landsat Collection 1 metadata
