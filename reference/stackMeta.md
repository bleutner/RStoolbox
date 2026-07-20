# Import separate Landsat files into single stack

Reads Landsat MTL or XML metadata files and loads single Landsat Tiffs
into a rasterStack. Be aware that by default stackMeta() does NOT import
panchromatic bands nor thermal bands with resolutions != 30m.

## Usage

``` r
stackMeta(file, quantity = "all", category = "image", allResolutions = FALSE)
```

## Arguments

- file:

  Character. Path to Landsat MTL metadata (\*\_MTL.txt) file or an
  Landsat CDR xml metadata file (\*.xml).

- quantity:

  Character vector. Which quantity should be returned. Options: digital
  numbers ('dn'), top of atmosphere reflectance ('tre'), at surface
  reflectance ('sre'), brightness temperature ('bt'), spectral index
  ('index'), all quantities ('all').

- category:

  Character vector. Which category of data to return. Options 'image':
  image data, 'pan': panchromatic image, 'index': multiband indices,
  'qa' quality flag bands, 'all': all categories.

- allResolutions:

  Logical. if `TRUE` a list will be returned with length = unique
  spatial resolutions. This argument was introduced to maintain backward
  compatibility and will be switched to TRUE in an upcoming release.
  Please base all new code on terra.

## Value

Returns one single SpatRaster comprising all requested bands. If
`allResolutions = TRUE` \*and\* there are different resolution layers
(e.g. a 15m panchromatic band along wit 30m imagery) a list of
RasterStacks will be returned.

## Note

Be aware that by default stackMeta() does NOT import panchromatic bands
nor thermal bands with resolutions != 30m. Use the allResolutions
argument to import all layers. Note that nowadays the USGS uses cubic
convolution to resample the TIR bands to 30m resolution.

## Examples

``` r
## Example metadata file (MTL)
mtlFile  <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")

## Read metadata
metaData <- readMeta(mtlFile)
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

## Load rasters based on metadata file
lsat     <- stackMeta(mtlFile)
lsat
#> class       : SpatRaster
#> size        : 310, 287, 7  (nrow, ncol, nlyr)
#> resolution  : 30, 30  (x, y)
#> extent      : 619395, 628005, -419505, -410205  (xmin, xmax, ymin, ymax)
#> coord. ref. : WGS 84 / UTM zone 22N (EPSG:32622)
#> sources     : LT52240631988227CUB02_B1.TIF
#>               LT52240631988227CUB02_B2.TIF
#>               LT52240631988227CUB02_B3.TIF
#>               ... and 4 more sources
#> names       : B1_dn, B2_dn, B3_dn, B4_dn, B5_dn, B6_dn, ...
#> min values  :    54,    18,    11,     4,     2,   131, ...
#> max values  :   185,    87,    92,   127,   148,   146, ...
```
