# Encode QA Conditions to Integers

Intended for use with Landsat 16-bit QA bands. Converts pixel quality
flags from human readable to integer, which can then be used to subset a
QA image. Please be aware of the default settings which differ for
different parameters. Depending on, which `sensor` and `legacy` is
selected, some quality parameters are not used, since the sequences of
available bitwise quality designations differ per sensor and collection.

## Usage

``` r
encodeQA(
  fill = "no",
  terrainOcclusion = "no",
  radSaturation = "na",
  cloudMask = "all",
  cloud = "all",
  cloudShadow = "all",
  snow = "all",
  cirrus = "all",
  droppedPixel = "no",
  water = "all",
  droppedFrame = "no",
  sensor = "OLI",
  legacy = "collection1"
)
```

## Arguments

- fill:

  Designated fill. Options: `c("yes", "no", "all")`.

- terrainOcclusion:

  Terrain induced occlusion. Options: `c("yes", "no", "all")`.

- radSaturation:

  Number of bands that contain radiometric saturation. Options:
  `c("na", "low", "med", "high", "all")` for no bands, 1-2 bands, 3-4
  bands, 5 or more bands contain saturation.

- cloudMask:

  Cloud mask. Options: `c("yes", "no", "all")`.

- cloud:

  Cloud confidence. Options: `c("na", "low", "med", "high", "all")`.

- cloudShadow:

  Cloud shadow confidence. Options: `c("yes", "no", "all")`.

- snow:

  Snow / ice confidence. Options:
  `c("na", "low", "med", "high", "all")`.

- cirrus:

  Cirrus confidence. Options: `c("na", "low", "med", "high", "all")`.

- droppedPixel:

  Dropped pixel. Options: `c("yes", "no", "all")`.

- water:

  Water confidence. Options: `c("na", "low", "med", "high", "all")`.

- droppedFrame:

  Dropped frame. Options: `c("yes", "no", "all")`.

- sensor:

  Sensor to encode. Options: `c("OLI", "TIRS", "ETM+", "TM", "MSS")`.

- legacy:

  Encoding systematic Options: `c("collection1", "pre_collection")`.
  Default is "collection1" for the Landsat Collection 1 8-bit quality
  designations. Use "pre_collection" for imagery downloaded before the
  Collection 1 quality designations were introduced

## Value

Returns the Integer value for the QA values

## Note

Only currently populated bits are available as arguments.

## References

<https://www.usgs.gov/landsat-missions/landsat-collection-1-level-1-quality-assessment-band>
for Collection 1 quality designations (`legacy = "collection1"`)

## Examples

``` r
encodeQA(snow = "low", cirrus = c("med", "high"), cloud = "high")
#> [1] 23136 23264 23392 23520 23152 23280 23408 23536
```
