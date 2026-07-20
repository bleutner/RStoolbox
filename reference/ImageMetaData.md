# ImageMetaData Class

ImageMetaData Class

## Usage

``` r
ImageMetaData(
  file = NA,
  format = NA,
  sat = NA,
  sen = NA,
  scene = NA,
  colNum = NA,
  colTier = NA,
  proj = NA,
  date = NA,
  pdate = NA,
  path = NA,
  row = NA,
  az = NA,
  selv = NA,
  esd = NA,
  files = NA,
  bands = NA,
  quant = NA,
  cat = NA,
  na = NA,
  vsat = NA,
  scal = NA,
  dtyp = NA,
  calrad = NA,
  calref = NA,
  calbt = NA,
  radRes = NA,
  spatRes = NA
)
```

## Arguments

- file:

  Character. Metadata file

- format:

  Character. Metadata format, e.g. xml, mtl

- sat:

  Character. Satellite platform

- sen:

  Character. Sensor

- scene:

  Character. Scene_ID

- colNum:

  Character Collection number

- colTier:

  Character Collection tier

- proj:

  CRS. Projection.

- date:

  POSIXct. Aquisition date.

- pdate:

  POSIXct. Processing date.

- path:

  Integer. Path.

- row:

  Integer. Row.

- az:

  Numeric. Sun azimuth

- selv:

  Numeric. Sun elevation

- esd:

  Numeric. Earth-sun distance

- files:

  Character vector. Files containing the data, e.g. tiff files

- bands:

  Character vector. Band names

- quant:

  Character vector. Quantity, one of c("dn", "tra", "tre", "sre", "bt",
  "idx", "angle")

- cat:

  Character vector. Category, e.g. c("image", "pan", "index", "qa",
  "aux")

- na:

  Numeric vector. No-data value per band

- vsat:

  Numeric vector. Saturation value per band

- scal:

  Numeric vector. Scale factor per band. e.g. if data was scaled to
  1000\*reflectance for integer conversion.

- dtyp:

  Character vector. Data type per band.

- calrad:

  data.frame. Calibration coefficients for dn-\>radiance conversion.
  Must have columns 'gain' and 'offset'. Rows named according to
  `bands`.

- calref:

  data.frame. Calibration coefficients for dn-\>reflectance conversion.
  Must have columns 'gain' and 'offset'. Rows named according to
  `bands`.

- calbt:

  data.frame. Calibration coefficients for dn-\>brightness temperature
  conversion. Must have columns 'K1' and 'K2'. Rows named according to
  `bands`.

- radRes:

  Numeric vector. Radiometric resolution per band.

- spatRes:

  Numeric vector. Spatial resolution per band.

## Value

Returns a structured, fully customizable meta-data table of a file
