# Multi-layer Pixel Entropy

Shannon entropy is calculated for each pixel based on it's layer values.
To be used with categorical / integer valued rasters.

## Usage

``` r
rasterEntropy(img, ...)
```

## Arguments

- img:

  SpatRaster

- ...:

  additional arguments passed to writeRaster

## Value

SpatRaster "entropy"

## Details

Entropy is calculated as -sum(p log(p)); p being the class frequency per
pixel.

## Examples

``` r
re <- rasterEntropy(rlogo)
ggR(re, geom_raster = TRUE)
```
