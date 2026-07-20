# Normalize Raster Images: Center and Scale

For each pixel subtracts the mean of the raster layer and optionally
divide by its standard deviation.

## Usage

``` r
normImage(img, norm = TRUE, ...)
```

## Arguments

- img:

  SpatRaster. Image to transform. Transformation will be performed
  separately for each layer.

- norm:

  Logical. Perform normalization (scaling) in addition to centering,
  i.e. divide by standard deviation.

- ...:

  further arguments passed to
  [writeRaster](https://rspatial.github.io/terra/reference/writeRaster.html).

## Value

Returns a SpatRaster with the same number layers as input layers with
each layer being centered and optionally normalized.

## Examples

``` r
library(terra)
## Load example data

## Normalization: Center and Scale
rlogo_center_norm <- normImage(rlogo)
hist(rlogo_center_norm)


## Centering
rlogo_center <- normImage(rlogo, norm = FALSE)
```
