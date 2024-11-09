# RStoolbox <img src="man/figures/logo.png" align="right" width="150" />

[![CI](https://github.com/bleutner/RStoolbox/actions/workflows/rcmdcheck.yaml/badge.svg)](https://github.com/bleutner/RStoolbox/actions/workflows/rcmdcheck.yaml)
[![CRAN version](https://www.r-pkg.org/badges/version/RStoolbox)](https://CRAN.R-project.org/package=RStoolbox)
[![codecov](https://codecov.io/gh/bleutner/RStoolbox/branch/master/graph/badge.svg)](https://app.codecov.io/gh/bleutner/RStoolbox)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/RStoolbox)](https://cranlogs.r-pkg.org/RStoolbox)

`RStoolbox` is an R package providing a wide range of tools for your every-day remote sensing processing needs. The available tool-set covers many aspects from data import, pre-processing, data analysis, image classification and graphical display. `RStoolbox` builds upon the `terra` package, which makes it suitable for processing large data-sets even on smaller workstations.

Find out more on the [`RStoolbox` webpage](https://bleutner.github.io/RStoolbox/).

## Installation
The package is available on CRAN and can be installed as usual via

```R
install.packages("RStoolbox")
```

To install the latest version from GitHub you need to have r-base-dev (Linux) or Rtools (Windows) installed.
Then run the following lines:

```R
library(devtools)
install_github("bleutner/RStoolbox")
```
    
## Get started

`RStoolbox` implements a variety of [remote sensing methods and workflows](https://bleutner.github.io/RStoolbox/reference/RStoolbox.html). Below are a few examples to get started. Further examples can be found in the [documentation of the respective functions](https://bleutner.github.io/RStoolbox/reference/index.html).

### Example 1: Classifications

The example below shows an unsupervised classification workflow based on *kmeans* clustering:

``` r
library(RStoolbox)

# unsupervised classification with 3 classes
uc <- unsuperClass(lsat, nClasses = 3)

# plot result using ggplot
ggR(uc$map, geom_raster = T, forceCat = T) +
  scale_fill_manual(values = c("darkgreen", "blue", "sandybrown"))
```

![](https://i.imgur.com/aUqzcxV.png)<!-- -->


If training data are available, e.g. labeled polygons, `RStoolbox` can be used to conduct a supervised classification. The workflow below employs *randomForest* to train a classification model:

``` r
library(RStoolbox)
library(caret)
library(randomForest)
library(ggplot2)
library(terra)

# example: training data from digitized polygons
train <- readRDS(system.file("external/trainingPolygons_lsat.rds", package="RStoolbox"))

# plot input data
ggRGB(lsat, r = 3, g = 2, b=1, stretch = "lin") +
  geom_sf(data = train, aes(fill = class)) + 
  scale_fill_manual(values = c("yellow", "sandybrown", "darkgreen", "blue"))
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
```

![](https://i.imgur.com/s071ieD.png)<!-- -->

``` r

# fit random forest (splitting training into 70\% training data, 30\% validation data)
sc <- superClass(lsat, trainData = train, responseCol = "class",
                 model = "rf", tuneLength = 1, trainPartition = 0.7)

# print model performance and confusion matrix
sc$modelFit
#> [[1]]
#>   TrainAccuracy TrainKappa method
#> 1     0.9992293  0.9988032     rf
#> 
#> [[2]]
#> Cross-Validated (5 fold) Confusion Matrix 
#> 
#> (entries are average cell counts across resamples)
#>  
#>             Reference
#> Prediction   cleared fallen_dry forest water
#>   cleared      141.6        0.0    0.0   0.0
#>   fallen_dry     0.0       22.0    0.0   0.0
#>   forest         0.4        0.0  255.0   0.0
#>   water          0.0        0.0    0.0  99.4
#>                             
#>  Accuracy (average) : 0.9992

# plotting: convert class IDs to class labels (factorize) and plot
r <- as.factor(sc$map)
levels(r) <- data.frame(ID = 1:4, class_supervised = levels(train$class))
ggR(r, geom_raster = T, forceCat = T) + scale_fill_manual(values = c("yellow", "sandybrown", "darkgreen", "blue"))
```

![](https://i.imgur.com/uYgRj03.png)<!-- -->

<sup>Created on 2024-04-19 with [reprex v2.1.0](https://reprex.tidyverse.org)</sup>

### Example 2: Spectral Unmixing

`RStoolbox` offers spectral unmixing by implementing the Multiple Endmember Spectral Mixture Analysis (MESMA) approach for estimating fractions of spectral classes, such as spectra of surfaces or materials, on a sub-pixel scale. The following workflow shows a simple Spectral Mixture Analysis (SMA) with single endmembers per class, extracted from the `lsat` example image by cell id:

``` r
library(RStoolbox)
library(terra)

#  to perform a SMA, use a single endmember per class, row by row:
em <- data.frame(lsat[c(5294, 47916)])
rownames(em) <- c("forest", "water")

# umix the lsat image
probs <- mesma(img = lsat, em = em)
plot(probs)
```

![](https://i.imgur.com/OqZVYc2.png)<!-- -->

Instead, one can define multiple endmembers per class to conduct a Multiple Endmember Spectral Mixture Analysis (MESMA):

``` r
library(RStoolbox)
library(terra)


# to perform a MESMA, use multiple endmembers per class, differntiating them
# by a column named 'class':
em <- rbind(
  data.frame(lsat[c(4155, 17018, 53134, 69487, 83704)], class = "forest"),
  data.frame(lsat[c(22742, 25946, 38617, 59632, 67313)], class = "water")
)

# unmix the lsat image
probs <- mesma(img = lsat, em = em)
plot(probs)
```

![](https://i.imgur.com/0PQGZa2.png)<!-- -->

``` r
# MESMA can also be performed on more than two endmember classes:
em <- rbind(
  data.frame(lsat[c(4155, 17018, 53134, 69487, 83704)], class = "forest"),
  data.frame(lsat[c(22742, 25946, 38617, 59632, 67313)], class = "water"),
  data.frame(lsat[c(4330, 1762, 1278, 1357, 17414)], class = "shortgrown")
)

# unmix the lsat image
probs <- mesma(img = lsat, em = em)
plot(probs)
```

![](https://i.imgur.com/a7QACjl.png)<!-- -->

### Example 3: Cloud Masking

`RStoolbox` comes with a suite of pre-processing functions, including `cloudMask` to identify clouds in optical satellite imagery:

``` r
library(ggplot2)

# lsat example scene, with two tiny clouds in the east
ggRGB(lsat, stretch = "lin")
```

![](https://i.imgur.com/NJ86OKE.png)<!-- -->

``` r
# calculate cloud index
cldmsk    <- cloudMask(lsat, blue = 1, tir = 6)
ggR(cldmsk, 2, geom_raster = TRUE) 
```

![](https://i.imgur.com/bvgUd74.png)<!-- -->

``` r
# mask by threshold, region-growing around the core cloud pixels
cldmsk_final <- cloudMask(cldmsk, threshold = 0.1, buffer = 5) 

## plot cloudmask 
ggRGB(lsat, stretch = "lin") +
  ggR(cldmsk_final[[1]], ggLayer = TRUE, forceCat = TRUE, geom_raster = TRUE) +
  scale_fill_manual(values = c("red"), na.value = NA)
#> Warning: Removed 88752 rows containing missing values or values outside the scale range
#> (`geom_raster()`).
```

![](https://i.imgur.com/wwjXK3v.png)<!-- -->


### Example 4: Radiometric and atmospheric correction

With `radCor`, users can compute radiometric and simple atmospheric corrections (based on dark object substraction):


``` r
library(terra)

# import Landsat meta data
mtlFile  <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")
metaData <- readMeta(mtlFile)
lsat_t <- stackMeta(mtlFile)

# convert DN to top of atmosphere reflectance and brightness temperature
lsat_ref <- radCor(lsat_t, metaData = metaData, method = "apref")

# correct DN to at-surface-reflecatance with DOS (Chavez decay model)
lsat_sref <- radCor(lsat_t, metaData = metaData)

# correct DN to at-surface-reflecatance with simple DOS and automatic haze estimation
hazeDN    <- estimateHaze(lsat_t, hazeBands = 1:4, darkProp = 0.01, plot = FALSE)
lsat_sref <- radCor(lsat_t, metaData = metaData, method = "sdos",
                    hazeValues = hazeDN, hazeBands = 1:4)

# plot result
ggRGB(lsat_sref, r = 3, g = 2, b = 1, stretch = "lin")
```

![](https://i.imgur.com/IEi9own.png)<!-- -->


<sup>Created on 2024-04-19 with [reprex v2.1.0](https://reprex.tidyverse.org)</sup>

