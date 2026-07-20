# Classify Landsat QA bands

extracts five classes from QA band: background, cloud, cirrus, snow and
water.

## Usage

``` r
classifyQA(
  img,
  type = c("background", "cloud", "cirrus", "snow", "water"),
  confLayers = FALSE,
  sensor = "OLI",
  legacy = "collection1",
  ...
)
```

## Arguments

- img:

  SpatRaster. Landsat 8 OLI QA band.

- type:

  Character. Classes which should be returned. One or more of
  c("background", "cloud", "cirrus","snow", "water").

- confLayers:

  Logical. Return one layer per class classified by confidence levels,
  i.e. cloud:low, cloud:med, cloud:high.

- sensor:

  Sensor to encode. Options: `c("OLI", "TIRS", "ETM+", "TM", "MSS")`.

- legacy:

  Encoding systematic Options: `c("collection1", "pre_collection")`.
  Default is "collection1" for the Landsat Collection 1 8-bit quality
  designations. Use "pre_collection" for imagery downloaded before the
  Collection 1 quality designations were introduced

- ...:

  further arguments passed to
  [writeRaster](https://rspatial.github.io/terra/reference/writeRaster.html)

## Value

Returns a SpatRaster with maximal five classes:

|            |       |
|------------|-------|
| class      | value |
| background | 1L    |
| cloud      | 2L    |
| cirrus     | 3L    |
| snow       | 4L    |
| water      | 5L    |

Values outside of these classes are returned as NA. If
`confLayers = TRUE` then a RasterStack with one layer per condition
(except 'background') is returned, whereby each layer contains the
confidence level of the condition.

|            |       |
|------------|-------|
| Confidence | value |
| low        | 1L    |
| med        | 2L    |
| high       | 3L    |

## Details

By default each class is queried for \*high\* confidence. See
[encodeQA](https://bleutner.github.io/RStoolbox/reference/encodeQA.md)
for details. To return the different confidence levels per condition use
`confLayers=TRUE`. This approach corresponds to the way LandsatLook
Quality Images are produced by the USGS.

## See also

[encodeQA](https://bleutner.github.io/RStoolbox/reference/encodeQA.md)
[decodeQA](https://bleutner.github.io/RStoolbox/reference/decodeQA.md)

## Examples

``` r
library(terra)
#> terra 1.9.34
qa <- rast(ncol = 100, nrow=100, val = sample(1:2^14,  10000))

## QA classes
qacs <- classifyQA(img = qa)
## Confidence levels
qacs_conf <- classifyQA(img = qa, confLayers = TRUE)
```
