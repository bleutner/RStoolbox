This is a patch release, which fixes current CRAN errors on multiple platforms due to updated caret package.

### CRAN checks
### Test environments
* Ubuntu 16.10 64bit (release)
* Debian 8.1 (devel, clang/UBSAN and VALGRIND) 
* Ubuntu 12.04 (on travis-ci), (oldrel, release, devel)
* Win-builder (release, devel)

### R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:
Possibly mis-spelled words in DESCRIPTION:
  indices (11:26)

This is a false alarm; 'indices' is spelled correctly.  

### Downstream dependencies
none


### Changelog RStoolbox 0.1.6
Fixes:
* fixes current CRAN errors on multiple platforms due to updated caret package