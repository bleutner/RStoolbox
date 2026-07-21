# Topographic Illumination Correction

account and correct for changes in illumination due to terrain
elevation.

## Usage

``` r
topCor(
  img,
  dem,
  metaData,
  solarAngles = c(),
  method = "C",
  stratImg = NULL,
  nStrat = 5,
  illu,
  ...
)
```

## Arguments

- img:

  SpatRaster. Imagery to correct

- dem:

  SpatRaster. Either a digital elevation model as a RasterLayer or a
  RasterStack/Brick with pre-calculated slope and aspect (see
  [terrain](https://rspatial.github.io/terra/reference/terrain.html)) in
  which case the layers must be named 'slope' and 'aspect'. Must have
  the same dimensions as `img`.

- metaData:

  Character, ImageMetaData. Either a path to a Landsat meta-data file
  (MTL) or an ImageMetaData object (see
  [readMeta](https://bleutner.github.io/RStoolbox/reference/readMeta.md))

- solarAngles:

  Numeric vector containing sun azimuth and sun zenith (in radians and
  in that order). Not needed if metaData is provided

- method:

  Character. One of c("cos", "avgcos", "minnaert", "C", "stat", "illu").
  Choosing 'illu' will return only the local illumination map.

- stratImg:

  RasterLayer or SpatRaster to define strata, e.g. NDVI. Or the string
  'slope' in which case stratification will be on `nStrat` slope
  classes. Only relevant if `method = 'minnaert'`.

- nStrat:

  Integer. Number of bins or quantiles to stratify by. If a bin has less
  than 50 samples it will be merged with the next bin. Only relevant if
  `method = 'minnaert'`.

- illu:

  SpatRaster. Optional pre-calculated ilumination map. Run topCor with
  method="illu" to calculate an ilumination map

- ...:

  arguments passed to
  [`writeRaster`](https://rspatial.github.io/terra/reference/writeRaster.html)

## Value

SpatRaster

## Details

For detailed discussion of the various approaches please see Riano et
al. (2003).

The minnaert correction can be stratified for different landcover
characteristics. If `stratImg = 'slope'` the analysis is stratified by
the slope, i.e. the slope values are divided into `nStrat` classes and
the correction coefficient k is calculated and applied separately for
each slope class. An alternative could be to stratify by a vegetation
index in which case an additional raster layer has to be provided via
the `stratImg` argument.

## References

Riano et al. (2003) Assessment of different topographic correction in
Landsat-TM data for mapping vegetation types. IEEE Transactions on
Geoscience and Remote Sensing.

## Examples

``` r
## Load example data
metaData <- system.file("external/landsat/LT52240631988227CUB02_MTL.txt", package="RStoolbox")
metaData <- readMeta(metaData)

## Minnaert correction, solar angles from metaData
lsat_minnaert <- topCor(lsat, dem = srtm, metaData = metaData, method = "minnaert")
#> 14:58:02 | Calculate slope and aspect
#> 14:58:02 | Calculate illumination map
#> 14:58:02 | Correct imagery
#> 14:58:03 | Estimate coefficients

## C correction, solar angles provided manually
lsat_C <- topCor(lsat, dem = srtm, solarAngles = c(1.081533, 0.7023922), method = "C")
#> 14:58:03 | Calculate slope and aspect
#> 14:58:03 | Calculate illumination map
#> 14:58:03 | Correct imagery
#> 14:58:03 | Estimate coefficients
```
