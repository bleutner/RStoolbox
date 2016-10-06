# RStoolbox

[![Build Status](https://travis-ci.org/bleutner/RStoolbox.svg)](https://travis-ci.org/bleutner/RStoolbox)
[![CRAN version](http://www.r-pkg.org/badges/version/RStoolbox)](https://CRAN.R-project.org/package=RStoolbox)

RStoolbox is an R package providing a wide range of tools for your every-day remote sensing processing needs. The available tool-set covers many aspects from data import, pre-processing, data analysis, image classification and graphical display. RStoolbox builds upon the raster package, which makes it suitable for processing large data-sets even on smaller workstations. Moreover in most parts decent support for parallel processing is implemented.

For more details have a look at the [functions overview](http://bleutner.github.io/RStoolbox/rstbx-docu/RStoolbox.html).

## Installation
The package is now on CRAN and can be installed as usual via

    install.packages("RStoolbox")


To install the latest version from GitHub you need to have r-base-dev (Linux) or Rtools (Windows) installed.
Then run the following lines:

    library(devtools)
    install_github("bleutner/RStoolbox")
    
