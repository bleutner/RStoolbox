# Extract bandwise information from ImageMetaData

This is an accessor function to quickly access information stored in
ImageMetaData, e.g. scale factor per band. Intended for use with imagery
which was imported using stackMeta. Will return parameters using the
actual band order in img.

## Usage

``` r
getMeta(img, metaData, what)
```

## Arguments

- img:

  SpatRaster or character vector with band names.

- metaData:

  ImageMetaData or path to meta data file.

- what:

  Character. Parameter to extract. Either data descriptors, or
  conversion parameters (see Details for options)

## Value

If `what` is one of `c('CALRAD', 'CALBT', 'CALREF')` a data.frame is
returned with bands in rows (order corresponding to `img` band order).
Otherwise a named numeric vector with the corresponding parameter is
returned (layernames as names).

## Details

Possible metadata parameters (`what` argument):

Data descriptors

|                      |     |
|----------------------|-----|
| 'FILES'              |     |
| 'QUANTITY'           |     |
| 'CATEGORY'           |     |
| 'NA_VALUE'           |     |
| 'SATURATE_VALUE'     |     |
| 'SCALE_FACTOR'       |     |
| 'DATA_TYPE'          |     |
| 'SPATIAL_RESOLUTION' |     |

Conversion parameters

|          |                                                               |
|----------|---------------------------------------------------------------|
| 'CALRAD' | Conversion parameters from DN to radiance                     |
| 'CALBT'  | Conversion parameters from radiance to brightness temperature |
| 'CALREF' | Conversion parameters from DN to reflectance (Landsat 8 only) |

## Examples

``` r
## Import example data
mtlFile  <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")
meta <- readMeta(mtlFile)
lsat_t <- stackMeta(mtlFile)

## Get integer scale factors
getMeta(lsat_t, metaData = meta, what = "SCALE_FACTOR")
#> B1_dn B2_dn B3_dn B4_dn B5_dn B6_dn B7_dn 
#>     1     1     1     1     1     1     1 

## Conversion factors for brightness temperature
getMeta("B6_dn", metaData = meta, what = "CALBT")
#>           K1      K2
#> B6_dn 607.76 1260.56

## Conversion factors to top-of-atmosphere radiance
## Band order not corresponding to metaData order
getMeta(lsat_t[[5:1]], metaData = meta, what = "CALRAD")
#>         offset  gain
#> B5_dn -0.49035 0.120
#> B4_dn -2.38602 0.876
#> B3_dn -2.21398 1.044
#> B2_dn -4.16220 1.322
#> B1_dn -2.19134 0.671

## Get integer scale factors
getMeta(lsat_t, metaData = meta, what = "SCALE_FACTOR")
#> B1_dn B2_dn B3_dn B4_dn B5_dn B6_dn B7_dn 
#>     1     1     1     1     1     1     1 

## Get file basenames
getMeta(lsat_t, metaData = meta, what = "FILES")
#>                          B1_dn                          B2_dn 
#> "LT52240631988227CUB02_B1.TIF" "LT52240631988227CUB02_B2.TIF" 
#>                          B3_dn                          B4_dn 
#> "LT52240631988227CUB02_B3.TIF" "LT52240631988227CUB02_B4.TIF" 
#>                          B5_dn                          B6_dn 
#> "LT52240631988227CUB02_B5.TIF" "LT52240631988227CUB02_B6.TIF" 
#>                          B7_dn 
#> "LT52240631988227CUB02_B7.TIF" 
```
