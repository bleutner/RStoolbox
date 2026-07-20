# Linear Image Rescaling

performs linear shifts of value ranges either to match min/max of
another image (`y`) or to any other min and max value (`ymin` and
`ymax`).

## Usage

``` r
rescaleImage(x, y, xmin, xmax, ymin, ymax, forceMinMax = FALSE, ...)
```

## Arguments

- x:

  SpatRaster or numeric vector. Image to normalise.

- y:

  SpatRaster or numeric vector. Reference image. Optional. Used to
  extract min and max values if ymin or ymax are missing.

- xmin:

  Numeric. Min value of x. Either a single value or one value per layer
  in x. If xmin is not provided it will be extracted from x.

- xmax:

  Numeric. Max value of x. Either a single value or one value per layer
  in x. If xmax is not provided it will be extracted from x.

- ymin:

  Numeric. Min value of y. Either a single value or one value per layer
  in x. If ymin is not provided it will be extracted from y.

- ymax:

  Numeric. Max value of y. Either a single value or one value per layer
  in x. If ymax is not provided it will be extracted from y.

- forceMinMax:

  Logical. Forces update of min and max data slots in x or y.

- ...:

  additional arguments passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)

## Value

Returns a SpatRaster of the same dimensions as the input raster `x` but
shifted and stretched to the new limits.

## Details

Providing `xmin` and `xmax` values manually can be useful if the raster
contains a variable of a known, fixed value range, e.g. NDVI from -1 to
1 but the actual pixel values don't encompass this entire range. By
providing `xmin = -1` and `xmax = 1` the values can be rescaled to any
other range, e.g. 1 to 100 while comparability to other rescaled NDVI
scenes is retained.

## See also

[histMatch](https://bleutner.github.io/RStoolbox/reference/histMatch.md)

## Examples

``` r
lsat2 <- lsat - 1000
lsat2
#> class       : SpatRaster
#> size        : 310, 287, 7  (nrow, ncol, nlyr)
#> resolution  : 30, 30  (x, y)
#> extent      : 619395, 628005, -419505, -410205  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=utm +zone=22 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#> source(s)   : memory
#> names       : B1_dn, B2_dn, B3_dn, B4_dn, B5_dn, B6_dn, ...
#> min values  :  -946,  -982,  -989,  -996,  -998,  -869, ...
#> max values  :  -815,  -913,  -908,  -873,  -852,  -854, ...

## Rescale lsat2 to match original lsat value range
lsat2_rescaled <- rescaleImage(lsat2, lsat)
lsat2_rescaled
#> class       : SpatRaster
#> size        : 310, 287, 7  (nrow, ncol, nlyr)
#> resolution  : 30, 30  (x, y)
#> extent      : 619395, 628005, -419505, -410205  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=utm +zone=22 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#> source(s)   : memory
#> names       : B1_dn, B2_dn, B3_dn, B4_dn, B5_dn, B6_dn, ...
#> min values  :    54,    18,    11,     4,     2,   131, ...
#> max values  :   185,    87,    92,   127,   148,   146, ...

## Rescale lsat to value range [0,1]
lsat2_unity <- rescaleImage(lsat2, ymin = 0, ymax = 1)
lsat2_unity
#> class       : SpatRaster
#> size        : 310, 287, 7  (nrow, ncol, nlyr)
#> resolution  : 30, 30  (x, y)
#> extent      : 619395, 628005, -419505, -410205  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=utm +zone=22 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#> source(s)   : memory
#> names       : B1_dn, B2_dn, B3_dn, B4_dn, B5_dn, B6_dn, ...
#> min values  :     0,     0,     0,     0,     0,     0, ...
#> max values  :     1,     1,     1,     1,     1,     1, ...
```
