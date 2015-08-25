# RStoolbox

[![Build Status](https://travis-ci.org/bleutner/RStoolbox.svg)](https://travis-ci.org/bleutner/RStoolbox)

RStoolbox is in initial development. We aim to provide fundamental remote sensing image processing and 
analysis tools for R, such as unsupervised and supervised classification or image regression (fractional cover).

## Installation
Please be aware that during initial development function and argument names are likely to change without notice.
Only once the package is submitted to CRAN they will remain stable.

Apart from R itself, you'll also need to have r-base-dev (Linux) or Rtools (Windows) installed.

    library(devtools)
    install_github("bleutner/RStoolbox")
    
If you are on Windows and don't have Rtools installed, you can install a  pre-compiled, but usually slightly outdated Windows binary build like so:

	install.packages("RStoolbox", repos=c("http://bleutner.github.io/RStoolbox/miniCRAN", "http://cran.r-project.org"))     