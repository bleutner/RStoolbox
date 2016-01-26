---
layout: post
title: "RStoolbox made it to CRAN"
description: ""
category: R
tags: [R,RStoolbox]
---
{% include JB/setup %}

![RStoolbox]({{BASE_PATH}}/assets/img/rstbx.png) After well over a year of (on-and-off) development we dared to knock on the menacing gates of CRAN.
Intensive testing and an intimate relationship with `R CMD check` payed out and with little further changes RStoolbox was accepted and can now be installed via `install.packages("RStoolbox")`. Hooray :-)

But not so fast! You think checking locally, checking with win-builder and checking with oldrel, release and devel on [Travis CI](https://travis-ci.org/bleutner/RStoolbox) would give you a CRAN proof jacket? Hahaha -- no.  
Quickly after the release I was told that the tests of RStoolbox failed the CRAN clang-UBSAN check. Something I had never even heard of, since I'm pretty much new to writing compiled code (some parts of RStoolbox are written in C++ thanks to the wonderful Rcpp package). For the record: it checks for undefined behavior in C++ code, for example trying to convert `NaN` to `int`. 

The first step -- to replicate the error -- turned out to be surprisingly hard. I followed this well written [blog post](http://francoismichonneau.net/2014/03/how-to-do-ubsan-tests-r-package/) but could not reproduce the error. Therefore, I fixed the part I suspected to be the culprit and resubmitted v0.1.1 to CRAN. It went smoothly and got accepted. Yet, the post-submission tests still fail (with clang only). 

Thus, I dug around some more and found Dirk Eddelbuettels [post](http://dirk.eddelbuettel.com/blog/2015/01/18/#ubsan-clang-container) in which he describes his [docker container of r-devel with UBSAN support](https://hub.docker.com/r/rocker/r-devel-ubsan-clang/). After going through some additional trouble to install gdal/ogr into the container I finally managed to reproduce and even later fix the issue (not submitted to CRAN yet). At least it gave me a reason to play with docker, which I had on my todo-list for some time now.   

Anyways, I'm proud to announce the initial release to CRAN. From now on function and argument names should remain mostly stable. 
We are looking forward to your comments, [suggestions](https://github.com/bleutner/RStoolbox/pulls) and [bug reports](https://github.com/bleutner/RStoolbox/issues). 

This initial release of RStoolbox ships with the following functions and data-sets:

### Data Import and Export

*   `readMeta`: import Landsat metadata from MTL or XML files

*   `stackMeta`: load Landsat bands based on metadata

*   `readSLI & writeSLI`: read and write ENVI spectral libraries

*   `saveRSTBX & readRSTBX`: save and re-import RStoolbox classification objects (model and map)

*   `readEE`: import and tidy EarthExplorer search results

### Data Pre-Processing

*   `radCor`: radiometric conversions and corrections. Primarily, yet not exclusively, intended for Landsat data processing. DN to radiance to reflectance conversion as well as DOS approaches

*   `topCor`: topographic illumination correction

*   `cloudMask & cloudShadowMask`: mask clouds and cloud shadows in Landsat or other imagery which comes with a thermal band

*   `classifyQA`: extract layers from Landsat 8 QA bands, e.g. cloud confidence

*   `rescaleImage`: rescale image to match min/max from another image or a specified min/max range

*   `normImage`: normalize imagery by centering and scaling

*   `histMatch`: matches the histograms of two scenes

*   `coregisterImages`: co-register images based on mutual information

*   `panSharpen`: sharpen a coarse resolution image with a high resolution image (typically panchromatic)

### Data Analysis

*   `spectralIndices`: calculate a set of predefined multispectral indices like NDVI

*   `tasseledCap`: tasseled cap transformation

*   `sam`: spectral angle mapper

*   `rasterPCA`: principal components transform for raster data

*   `rasterCVA`: change vector analysis

*   `unsuperClass`: unsupervised classification

*   `superClass`: supervised classification

*   `fCover`: fractional cover of coarse resolution imagery based on high resolution classificaton

### Data Display with ggplot2
*   `fortify.raster`: data.frame from raster (subsampled) for plotting

*   `ggR`: single raster layer plotting with ggplot2

*   `ggRGB`: efficient plotting of remote sensing imagery in RGB with ggplot2

### Example Data Sets

*   `rlogo`: the r logo as raster brick

*   `lsat`: subset of a Landsat 5 TM scene

*   `srtm`: SRTM DEM for lsat scene