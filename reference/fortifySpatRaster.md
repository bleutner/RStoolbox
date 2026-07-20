# Fortify method for classes from the terra package.

Fortify method for classes from the terra package.

## Usage

``` r
fortifySpatRaster(x, maxpixels = 50000)
```

## Arguments

- x:

  `SpatRaster` object to convert into a dataframe.

- maxpixels:

  Integer. Maximum number of pixels to sample

## Value

Returns a data.frame with coordinates (x,y) and corresponding raster
values.

## Examples

``` r
r_df <- fortifySpatRaster(rlogo)
head(r_df)
#>     x    y red green blue
#> 1 0.5 76.5 255   255  255
#> 2 1.5 76.5 255   255  255
#> 3 2.5 76.5 255   255  255
#> 4 3.5 76.5 255   255  255
#> 5 4.5 76.5 255   255  255
#> 6 5.5 76.5 255   255  255
```
