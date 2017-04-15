This is a minor update fixing unit tests which broke the downstream tests of the recently updated 'caret' package.
Also registration of native routines has been added.

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

### Changelog RStoolbox 0.1.8
New:
* `spectralIndices()` can now apply a mask internally, e.g. to exclude cloud pixels. New arguments are: 
   `maskLayer` and `maskValue` (suggested by Andrea Hess).   
* added spectral index GNDWI   

Fixes: 
* update `readEE()` to deal with new EarthExplorer export columns (reported by Christian Bauer)