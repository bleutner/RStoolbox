# Estimate Image Haze for Dark Object Subtraction (DOS)

estimates the digital number (DN) pixel value of \*dark\* objects for
the visible wavelength range.

## Usage

``` r
estimateHaze(
  x,
  hazeBands,
  darkProp = 0.01,
  maxSlope = TRUE,
  plot = FALSE,
  returnTables = FALSE
)
```

## Arguments

- x:

  SpatRaster or a previous result from `estimateHaze` with
  `returnTables = TRUE` from which to estimate haze

- hazeBands:

  Integer or Character. Band number or bandname from which to estimate
  atmospheric haze (optional if x contains only one layer)

- darkProp:

  Numeric. Proportion of pixels estimated to be dark.

- maxSlope:

  Logical. Use `darkProp` only as an upper boundary and search for the
  DN of maximum slope in the histogram below this value.

- plot:

  Logical. Option to display histograms and haze values

- returnTables:

  Logical. Option to return the frequency table per layer. Only takes
  effect if x is a SpatRaster. If x is a result of estimateHaze tables
  will always be returned.

## Value

If returnTables is FALSE (default). Then a vector of length(hazeBands)
containing the estimated haze DNs will be returned. If returnTables is
TRUE a list with two components will be returned. The list element 'SHV'
contains the haze values, while 'table' contains another list with the
sampled frequency tables. The latter can be re-used to try different
darkProp thresholds without having to sample the raster again.

## Details

It is assumed that any radiation originating from \*dark\* pixels is due
to atmospheric haze and not the reflectance of the surface itself (the
surface is dark, i.e. it has a reflectance close to zero). Hence, the
haze values are estimates of path radiance, which can be subtracted in a
dark object subtraction (DOS) procedure (see
[`radCor`](https://bleutner.github.io/RStoolbox/reference/radCor.md))

Atmospheric haze affects almost exclusively the visible wavelength
range. Therefore, typically, you'd only want to estimate haze in blue,
green and red bands, occasionally also in the nir band.

## Examples

``` r
## Estimate haze for blue, green and red band
haze <- estimateHaze(lsat, hazeBands = 1:3, plot = FALSE)
haze
#> B1_dn B2_dn B3_dn 
#>    55    19    12 

## Find threshold interactively
#### Return the frequency tables for re-use
#### avoids having to sample the Raster again and again
haze <- estimateHaze(lsat, hazeBands = 1:3, returnTables = TRUE)
## Use frequency table instead of lsat and fiddle with
haze <- estimateHaze(haze, hazeBands = 1:3, darkProp = .1, plot = FALSE)
haze$SHV
#> B1_dn B2_dn B3_dn 
#>    57    20    12 
```
