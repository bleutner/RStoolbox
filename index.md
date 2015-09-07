---
layout: page
title: "RStoolbox: Remote Sensing Data Analysis in R"
tagline: Supporting tagline
---
{% include JB/setup %}




#### The Package

**RStoolbox** is a R package providing a wide range of tools for your every-day remote sensing processing needs. 
The available toolset covers many aspects from data import, pre-processing, data analysis, image classification and graphical display. 
RStoolbox builds upon the raster package, which makes it suitable for processing large data-sets even on smaller workstations. 
Moreover in most parts decent support for parallel processing is implemented. The package is published under GPL3.
   
#### Installing RStoolbox

RStoolbox is now available from CRAN and can be installed as usual with

    install.packages("RStoolbox")

To get the latest development version, install RStoolbox directly from the [github repository](http://www.github.com/bleutner/RStoolbox). Apart from R itself, you'll also need to have r-base-dev (Linux) or [Rtools](http://cran.r-project.org/bin/windows/Rtools) (Windows) installed to compile the package.

    library(devtools)
    install_github("bleutner/RStoolbox")
  

#### Use the Source

You can view or fork the most recent code on my [GitHub repository](http://www.github.com/bleutner/RStoolbox) or grab the latest source tarball from [here](https://github.com/bleutner/RStoolbox/tarball/master).
    

#### Feedback

I am grateful for any feedback on RStoolbox. Please do not hesitate to report an issue [here](https://github.com/bleutner/RStoolbox/issues) if you discover bugs or have a feature idea.
If you find the package useful I'd love to hear about it as well. Just drop me an email if you like: benjamin.leutner@uni-wuerzburg.de.






