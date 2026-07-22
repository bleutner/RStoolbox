# Map accuracy assessment

validate a map from a classification or regression model. This can be
useful to update the accuracy assessment after filtering, e.g. for a
minimum mapping unit.

## Usage

``` r
validateMap(
  map,
  valData,
  responseCol,
  nSamplesV = 500,
  mode = "classification",
  classMapping = NULL
)
```

## Arguments

- map:

  SpatRaster. The classified map.

- valData:

  sf object with validation data (POLYGONs or POINTs).

- responseCol:

  Character. Column containing the validation data in attribute table of
  `valData`.

- nSamplesV:

  Integer. Number of pixels to sample for validation (only applies to
  polygons).

- mode:

  Character. Either 'classification' or 'regression'.

- classMapping:

  optional data.frame with columns `'class'` and `'classID'` defining
  the mapping from raster integers to class names.

## Value

Returns a structured list includng the preformance and confusion-matrix
of your then validated input data

## Examples

``` r
library(caret)
library(terra)

## Training data
poly     <- readRDS(system.file("external/trainingPolygons_lsat.rds", package="RStoolbox"))

## Split training data in training and validation set (50%-50%)
splitIn   <- createDataPartition(poly$class, p = .5)[[1]]
train <- poly[splitIn,]
val   <- poly[-splitIn,]

## Classify (deliberately poorly)
sc <- superClass(lsat, trainData = train, responseCol = "class", nSamples = 50, model = "mlc")
#> 10:19:26 | Begin sampling training data
#> 10:19:26 | Starting to fit model
#> 10:19:26 | Starting spatial predict
#> ******************** Model summary ********************
#> Maximum Likelihood Classification 
#> 
#> 274 samples
#>   7 predictor
#>   4 classes: 'cleared', 'fallen_dry', 'forest', 'water' 
#> 
#> No pre-processing
#> Resampling: Cross-Validated (5 fold) 
#> Summary of sample sizes: 219, 219, 219, 220, 219 
#> Resampling results:
#> 
#>   Accuracy  Kappa
#>   1         1    
#> 
#> [[1]]
#>   TrainAccuracy TrainKappa method
#> 1             1          1 custom
#> 
#> [[2]]
#> Cross-Validated (5 fold) Confusion Matrix 
#> 
#> (entries are average cell counts across resamples)
#>  
#>             Reference
#> Prediction   cleared fallen_dry forest water
#>   cleared       16.0        0.0    0.0   0.0
#>   fallen_dry     0.0        6.8    0.0   0.0
#>   forest         0.0        0.0   16.0   0.0
#>   water          0.0        0.0    0.0  16.0
#>                        
#>  Accuracy (average) : 1
#> 
#> 
#> ******************** Validation summary ********************
#> [1] "No independent validation was performed!"

## Polish map with majority filter

polishedMap <- focal(sc$map, matrix(1,3,3), fun = modal) 

## Validation
## Before filtering
val0 <- validateMap(sc$map, valData = val, responseCol = "class",
                            classMapping = sc$classMapping)
## After filtering
val1 <- validateMap(polishedMap, valData = val, responseCol = "class",
                             classMapping = sc$classMapping)
```
