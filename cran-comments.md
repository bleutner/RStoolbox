Fixes functions/tests for current R-devel (Email from CRAN/Ripley, 4.1.19)

### R CMD checks
### Test environments
* Ubuntu 18.04 64bit (release)
* Travis-CI Ubuntu 14.04 (devel)
* winbuilder (devel, release, oldrel)

### R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

### Downstream dependencies
moveVis OK
fieldRS OK


### Changelog:
RStoolbox 0.2.4
====================================
New:

* function `oneHotEncode()`: splits a single rasterLayer into multiple layers (one per class)
  with one-hot encoding, i.e. pixel value = 1 for matches, pixel value = 0 for all other classes (background).
* `ggR()` can now display more than one layer. Each layer can be plotted to a subplot in a multi-panel grafic.
* `encodeQA()`, `decodeQA()` and `classifyQA()` can now deal with the new QA format introduced with Landsat Collection data. Legacy QA designations can still be interpreted by setting the `legacy` argument.
* new `predict()` method for unsupervised classification models (`unsuperClass`).

Changes: 
* all `radCor()` DOS methods now work for Landsat 8 OLI

Fixes:
* fix `unsuperClass()` for algorithms other than Hartigan-Wong (reported by Alex Ilich)
