# RStoolbox: A Collection of Remote Sensing Tools

The RStoolbox package provides a set of functions which simplify
performing standard remote sensing tasks in R.

## Data Import and Export

- [`readMeta`](https://bleutner.github.io/RStoolbox/reference/readMeta.md):
  import Landsat metadata from MTL or XML files

- [`stackMeta`](https://bleutner.github.io/RStoolbox/reference/stackMeta.md),
  [`getMeta`](https://bleutner.github.io/RStoolbox/reference/getMeta.md):
  load Landsat bands based on metadata

- [`readSLI`](https://bleutner.github.io/RStoolbox/reference/readSLI.md)` & `[`writeSLI`](https://bleutner.github.io/RStoolbox/reference/writeSLI.md):
  read and write ENVI spectral libraries

- [`saveRSTBX`](https://bleutner.github.io/RStoolbox/reference/saveRSTBX.md)` & `[`readRSTBX`](https://bleutner.github.io/RStoolbox/reference/saveRSTBX.md):
  save and re-import RStoolbox classification objects (model and map)

- [`readEE`](https://bleutner.github.io/RStoolbox/reference/readEE.md):
  import and tidy EarthExplorer search results

## Data Pre-Processing

- [`radCor`](https://bleutner.github.io/RStoolbox/reference/radCor.md):
  radiometric conversions and corrections. Primarily, yet not
  exclusively, intended for Landsat data processing. DN to radiance to
  reflectance conversion as well as DOS approaches

- [`topCor`](https://bleutner.github.io/RStoolbox/reference/topCor.md):
  topographic illumination correction

- [`cloudMask`](https://bleutner.github.io/RStoolbox/reference/cloudMask.md)` & `[`cloudShadowMask`](https://bleutner.github.io/RStoolbox/reference/cloudShadowMask.md):
  mask clouds and cloud shadows in Landsat or other imagery which comes
  with a thermal band

- [`classifyQA`](https://bleutner.github.io/RStoolbox/reference/classifyQA.md):
  extract layers from Landsat 8 QA bands, e.g. cloud confidence

- [`encodeQA`](https://bleutner.github.io/RStoolbox/reference/encodeQA.md)` & `[`decodeQA`](https://bleutner.github.io/RStoolbox/reference/decodeQA.md):
  encode/decode Landsat 16-bit QA bands.

- [`rescaleImage`](https://bleutner.github.io/RStoolbox/reference/rescaleImage.md):
  rescale image to match min/max from another image or a specified
  min/max range

- [`normImage`](https://bleutner.github.io/RStoolbox/reference/normImage.md):
  normalize imagery by centering and scaling

- [`oneHotEncode`](https://bleutner.github.io/RStoolbox/reference/oneHotEncode.md):
  one-hot encode a raster or vector

- [`histMatch`](https://bleutner.github.io/RStoolbox/reference/histMatch.md):
  matches the histograms of two scenes

- [`pifMatch`](https://bleutner.github.io/RStoolbox/reference/pifMatch.md):
  matches one scene to another based on linear regression of
  Pseudo-Invariant Features (PIF)

- [`coregisterImages`](https://bleutner.github.io/RStoolbox/reference/coregisterImages.md):
  co-register images based on mutual information

- [`panSharpen`](https://bleutner.github.io/RStoolbox/reference/panSharpen.md):
  sharpen a coarse resolution image with a high resolution image
  (typically panchromatic)

- [`estimateHaze`](https://bleutner.github.io/RStoolbox/reference/estimateHaze.md):
  estimate image haze for Dark Object Subtraction (DOS)

## Data Analysis

- [`spectralIndices`](https://bleutner.github.io/RStoolbox/reference/spectralIndices.md):
  calculate a set of predefined multispectral indices like NDVI

- [`tasseledCap`](https://bleutner.github.io/RStoolbox/reference/tasseledCap.md):
  tasseled cap transformation

- [`sam`](https://bleutner.github.io/RStoolbox/reference/sam.md):
  spectral angle mapper

- [`rasterPCA`](https://bleutner.github.io/RStoolbox/reference/rasterPCA.md):
  principal components transform for raster data

- [`rasterCVA`](https://bleutner.github.io/RStoolbox/reference/rasterCVA.md):
  change vector analysis

- [`rasterEntropy`](https://bleutner.github.io/RStoolbox/reference/rasterEntropy.md):
  calculates shannon entropy

- [`unsuperClass`](https://bleutner.github.io/RStoolbox/reference/unsuperClass.md):
  unsupervised classification

- [`superClass`](https://bleutner.github.io/RStoolbox/reference/superClass.md),
  [`validateMap`](https://bleutner.github.io/RStoolbox/reference/validateMap.md),
  [`getValidation`](https://bleutner.github.io/RStoolbox/reference/getValidation.md):
  supervised classification and validation

- [`fCover`](https://bleutner.github.io/RStoolbox/reference/fCover.md):
  fractional cover of coarse resolution imagery based on high resolution
  classification

- [`mesma`](https://bleutner.github.io/RStoolbox/reference/mesma.md):
  spectral unmixing using Multiple Endmember Spectral Mixture Analysis
  (MESMA)

## Data Display

- [`ggR`](https://bleutner.github.io/RStoolbox/reference/ggR.md): single
  raster layer plotting with ggplot2

- [`ggRGB`](https://bleutner.github.io/RStoolbox/reference/ggRGB.md):
  efficient plotting of remote sensing imagery in RGB with ggplot2
