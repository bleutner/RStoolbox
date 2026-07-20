# Read Landsat MTL metadata files

Reads metadata and deals with legacy versions of Landsat metadata files
and where possible adds missing information (radiometric gain and
offset, earth-sun distance).

## Usage

``` r
readMeta(file, raw = FALSE)
```

## Arguments

- file:

  path to Landsat MTL file (...MTL.txt)

- raw:

  Logical. If `TRUE` the full raw metadata will be returned as a list.
  if `FALSE` (the default) all important metadata are homogenized into a
  standard format (ImageMetaData) and some information is added.

## Value

Object of class ImageMetaData

## Examples

``` r
## Example metadata file (MTL)
mtlFile  <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")

## Read metadata
metaData <- readMeta(mtlFile)

## Summary
summary(metaData)
#> Scene:      LT52240631988227CUB02 
#> Satellite:  LANDSAT5 
#> Sensor:     TM 
#> Date:       1988-08-14 
#> Path/Row:   224/63 
#> Projection: +proj=utm +zone=22 +units=m +datum=WGS84 +ellips=WGS84 
#> Scene:      PROJCRS["unknown",
#>     BASEGEOGCRS["unknown",
#>         DATUM["World Geodetic System 1984",
#>             ELLIPSOID["WGS 84",6378137,298.257223563,
#>                 LENGTHUNIT["metre",1]],
#>             ID["EPSG",6326]],
#>         PRIMEM["Greenwich",0,
#>             ANGLEUNIT["degree",0.0174532925199433],
#>             ID["EPSG",8901]]],
#>     CONVERSION["UTM zone 22N",
#>         METHOD["Transverse Mercator",
#>             ID["EPSG",9807]],
#>         PARAMETER["Latitude of natural origin",0,
#>             ANGLEUNIT["degree",0.0174532925199433],
#>             ID["EPSG",8801]],
#>         PARAMETER["Longitude of natural origin",-51,
#>             ANGLEUNIT["degree",0.0174532925199433],
#>             ID["EPSG",8802]],
#>         PARAMETER["Scale factor at natural origin",0.9996,
#>             SCALEUNIT["unity",1],
#>             ID["EPSG",8805]],
#>         PARAMETER["False easting",500000,
#>             LENGTHUNIT["metre",1],
#>             ID["EPSG",8806]],
#>         PARAMETER["False northing",0,
#>             LENGTHUNIT["metre",1],
#>             ID["EPSG",8807]],
#>         ID["EPSG",16022]],
#>     CS[Cartesian,2],
#>         AXIS["(E)",east,
#>             ORDER[1],
#>             LENGTHUNIT["metre",1,
#>                 ID["EPSG",9001]]],
#>         AXIS["(N)",north,
#>             ORDER[2],
#>             LENGTHUNIT["metre",1,
#>                 ID["EPSG",9001]]]]
#> 
#> Data:
#>                              FILES QUANTITY CATEGORY
#> B1_dn LT52240631988227CUB02_B1.TIF       dn    image
#> B2_dn LT52240631988227CUB02_B2.TIF       dn    image
#> B3_dn LT52240631988227CUB02_B3.TIF       dn    image
#> B4_dn LT52240631988227CUB02_B4.TIF       dn    image
#> B5_dn LT52240631988227CUB02_B5.TIF       dn    image
#> B6_dn LT52240631988227CUB02_B6.TIF       dn    image
#> B7_dn LT52240631988227CUB02_B7.TIF       dn    image
#> 
#> Available calibration parameters (gain and offset):
#>  dn -> radiance (toa)
#>  dn -> brightness temperature (toa)
#> 
```
