fixes current incompatibility with new caret version

### R CMD checks
### Test environments
* Ubuntu 17.10 64bit (release)
* Travis-CI Ubuntu 14.04 (devel)
* winbuilder (devel, release, oldrel)
* r-hub (devel+ubsan)

### R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:
Possibly mis-spelled words in DESCRIPTION:
  indices (10:26)

This is a false alarm; 'indices' is spelled correctly.  

### Downstream dependencies
none

### Changelog RStoolbox 0.2.0
New:
* function `mesma` for spectral unmixing (#33, provided by Jakob Schwalb-Willmann)

Fixes: 
* improved NA handling and faster implementation of mlc classifier (#32, pull request by Neal Fultz)
* adapt to upcoming caret version (new constraint caret >= 6.0-79)

