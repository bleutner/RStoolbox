# One-hot encode a raster or vector

Splits a categorical raster layer (or a vector) into a multilayer raster
(or matrix).

## Usage

``` r
oneHotEncode(img, classes, background = 0, foreground = 1, na.rm = FALSE, ...)
```

## Arguments

- img:

  SpatRaster or integer/numeric vector containing multiple classes

- classes:

  integer: vector of classes which should be extracted

- background:

  integer: background value (default = 0)

- foreground:

  integer: foreground value (default = 1)

- na.rm:

  logical: if `TRUE`, `NA`s will be coerced to the `background` value.

- ...:

  further arguments passed to
  [writeRaster](https://rspatial.github.io/terra/reference/writeRaster.html).
  Ignored if img is not a SpatRaster, but a numeric/integer vector or
  matrix

## Value

A SpatRaster with as many layers as there are classes. Pixels matching
the class of interest are set to 1, backround values by default are set
to 0 (see background argument)

## Examples

``` r
# \donttest{
sc <- unsuperClass(rlogo, nClasses = 3)

## one-hot encode 
sc_oneHot <- oneHotEncode(sc$map, classes = c(1,2,3))

## check results
sc_oneHot
#> class       : SpatRaster
#> size        : 77, 101, 3  (nrow, ncol, nlyr)
#> resolution  : 1, 1  (x, y)
#> extent      : 0, 101, 0, 77  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#> source(s)   : memory
#> names       : c_1, c_2, c_3
#> min values  :   0,   0,   0
#> max values  :   1,   1,   1
# }
```
