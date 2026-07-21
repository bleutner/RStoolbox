# Supervised Classification

Supervised classification both for classification and regression mode
based on vector training data (points or polygons).

## Usage

``` r
superClass(
  img,
  trainData,
  valData = NULL,
  responseCol = NULL,
  nSamples = 1000,
  nSamplesV = 1000,
  polygonBasedCV = FALSE,
  trainPartition = NULL,
  model = "rf",
  tuneLength = 3,
  kfold = 5,
  sampling = NULL,
  minDist = 2,
  mode = "classification",
  predict = TRUE,
  predType = "raw",
  filename = NULL,
  verbose,
  overwrite = TRUE,
  ...
)
```

## Arguments

- img:

  SpatRaster. Typically remote sensing imagery, which is to be
  classified.

- trainData:

  sf or sp spatial vector data containing the training locations
  (POINTs,or POLYGONs).

- valData:

  Ssf or sp spatial vector data containing the validation locations
  (POINTs,or POLYGONs) (optional).

- responseCol:

  Character or integer giving the column in `trainData`, which contains
  the response variable. Can be omitted, when `trainData` has only one
  column.

- nSamples:

  Integer. Number of samples per land cover class. If `NULL` all pixels
  covered by training polygons are used (memory intensive!). Ignored if
  trainData consists of POINTs.

- nSamplesV:

  Integer. Number of validation samples per land cover class. If `NULL`
  all pixels covered by validation polygons are used (memory
  intensive!). Ignored if valData consists of POINTs.

- polygonBasedCV:

  Logical. If `TRUE` model tuning during cross-validation is conducted
  on a per-polygon basis. Use this to deal with overfitting issues. Does
  not affect training data supplied as SpatialPointsDataFrames.

- trainPartition:

  Numeric. Partition (polygon based) of `trainData` that goes into the
  training data set between zero and one. Ignored if `valData` is
  provided.

- model:

  Character. Which model to use. See
  [train](https://rdrr.io/pkg/caret/man/train.html) for options.
  Defaults to randomForest ('rf'). In addition to the standard caret
  models, a maximum likelihood classification is available via
  `model = 'mlc'`.

- tuneLength:

  Integer. Number of levels for each tuning parameter (see
  [train](https://rdrr.io/pkg/caret/man/train.html) for details).

- kfold:

  Integer. Number of cross-validation resamples during model tuning.

- sampling:

  Character. Describes the type of additional sampling that is conducted
  after resampling (usually to resolve class imbalances), from caret.
  Currently supported are `up`, `down`, `smote`, and `rose`. Note, that
  `smote` requires the packages `themis` and `rose` the package `ROSE`.
  Latter is noly for binary classification problems.

- minDist:

  Numeric. Minumum distance between training and validation data, e.g.
  `minDist=1` clips validation polygons to ensure a minimal distance of
  one pixel (pixel size according to `img`) to the next training
  polygon. Requires all data to carry valid projection information.

- mode:

  Character. Model type: 'regression' or 'classification'.

- predict:

  Logical. Produce a map (TRUE, default) or only fit and validate the
  model (FALSE).

- predType:

  Character. Type of the final output raster. Either "raw" for class
  predictions or "prob" for class probabilities. Class probabilities are
  not available for all classification models
  ([predict.train](https://rdrr.io/pkg/caret/man/predict.train.html)).

- filename:

  Path to output file (optional). If `NULL`, standard raster handling
  will apply, i.e. storage either in memory or in the raster temp
  directory.

- verbose:

  Logical. prints progress and statistics during execution

- overwrite:

  logical. Overwrite spatial prediction raster if it already exists.

- ...:

  further arguments to be passed to
  [`train`](https://rdrr.io/pkg/caret/man/train.html)

## Value

A superClass object (effectively a list) containing:

1.  \$model: the fitted model

2.  \$modelFit: model fit statistics

3.  \$training: indexes of samples used for training

4.  \$validation: list of

    1.  \$performance: performance estimates based on independent
        validation (confusion matrix etc.)

    2.  \$validationSamples: actual pixel coordinates plus reference and
        predicted values used for validation

    3.  \$validationGeometry: validation polygpns (clipped with mindist
        to training geometries)

5.  \$map: the predicted raster

6.  \$classMapping: a data.frame containing an integer \<-\> label
    mapping

## Details

Note that superClass automatically loads the lattice and randomForest
package. SuperClass performs the following steps:

1.  Ensure non-overlap between training and validation data. This is
    neccesary to avoid biased performance estimates. A minimum distance
    (`minDist`) in pixels can be provided to enforce a given distance
    between training and validation data.

2.  Sample training coordinates. If `trainData` (and `valData` if
    present) are polygons `superClass` will calculate the area per
    polygon and sample `nSamples` locations per class within these
    polygons. The number of samples per individual polygon scales with
    the polygon area, i.e. the bigger the polygon, the more samples.

3.  Split training/validation If `valData` was provided (reccomended)
    the samples from these polygons will be held-out and not used for
    model fitting but only for validation. If `trainPartition` is
    provided the trainingPolygons will be divided into training polygons
    and validation polygons.

4.  Extract raster data The predictor values on the sample pixels are
    extracted from `img`

5.  Fit the model. Using caret::train on the sampled training data the
    `model` will be fit, including parameter tuning (`tuneLength`) in
    `kfold` cross-validation. `polygonBasedCV=TRUE` will define
    cross-validation folds based on polygons (reccomended) otherwise it
    will be performed on a per-pixel basis.

6.  Predict the classes of all pixels in `img` based on the final model.

7.  Validate the model with the independent validation data.

## See also

[`train`](https://rdrr.io/pkg/caret/man/train.html)

## Examples

``` r
library(RStoolbox)
library(caret)
library(randomForest)
#> randomForest 4.7-1.2
#> Type rfNews() to see new features/changes/bug fixes.
#> 
#> Attaching package: ‘randomForest’
#> The following object is masked from ‘package:gridExtra’:
#> 
#>     combine
#> The following object is masked from ‘package:ggplot2’:
#> 
#>     margin
library(e1071)
#> 
#> Attaching package: ‘e1071’
#> The following object is masked from ‘package:ggplot2’:
#> 
#>     element
#> The following object is masked from ‘package:terra’:
#> 
#>     interpolate
library(terra)
train <- readRDS(system.file("external/trainingPoints_rlogo.rds", package="RStoolbox"))

## Plot training data
olpar <- par(no.readonly = TRUE) # back-up par
par(mfrow=c(1,2))
colors <- c("yellow", "green", "deeppink")
plotRGB(rlogo)
plot(train, add = TRUE, col =  colors[train$class], pch = 19)

## Fit classifier (splitting training into 70\% training data, 30\% validation data)
SC       <- superClass(rlogo, trainData = train, responseCol = "class",
model = "rf", tuneLength = 1, trainPartition = 0.7)
#> 14:58:01 | Begin sampling training data
#> 14:58:01 | Starting to fit model
#> 14:58:01 | Starting spatial predict
#> 14:58:01 | Begin validation
#> ******************** Model summary ********************
#> Random Forest 
#> 
#> 21 samples
#>  3 predictor
#>  3 classes: 'A', 'B', 'C' 
#> 
#> No pre-processing
#> Resampling: Cross-Validated (5 fold) 
#> Summary of sample sizes: 16, 16, 18, 17, 17 
#> Resampling results:
#> 
#>   Accuracy  Kappa
#>   1         1    
#> 
#> Tuning parameter 'mtry' was held constant at a value of 1
#> [[1]]
#>   TrainAccuracy TrainKappa method
#> 1             1          1     rf
#> 
#> [[2]]
#> Cross-Validated (5 fold) Confusion Matrix 
#> 
#> (entries are average cell counts across resamples)
#>  
#>           Reference
#> Prediction   A   B   C
#>          A 1.4 0.0 0.0
#>          B 0.0 1.4 0.0
#>          C 0.0 0.0 1.4
#>                        
#>  Accuracy (average) : 1
#> 
#> 
#> ******************** Validation summary ********************
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction A B C
#>          A 3 0 0
#>          B 0 3 0
#>          C 0 0 3
#> 
#> Overall Statistics
#>                                      
#>                Accuracy : 1          
#>                  95% CI : (0.6637, 1)
#>     No Information Rate : 0.3333     
#>     P-Value [Acc > NIR] : 5.081e-05  
#>                                      
#>                   Kappa : 1          
#>                                      
#>  Mcnemar's Test P-Value : NA         
#> 
#> Statistics by Class:
#> 
#>                      Class: A Class: B Class: C
#> Sensitivity            1.0000   1.0000   1.0000
#> Specificity            1.0000   1.0000   1.0000
#> Pos Pred Value         1.0000   1.0000   1.0000
#> Neg Pred Value         1.0000   1.0000   1.0000
#> Prevalence             0.3333   0.3333   0.3333
#> Detection Rate         0.3333   0.3333   0.3333
#> Detection Prevalence   0.3333   0.3333   0.3333
#> Balanced Accuracy      1.0000   1.0000   1.0000
SC
#> superClass results
#> ************ Validation **************
#> $validation
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction A B C
#>          A 3 0 0
#>          B 0 3 0
#>          C 0 0 3
#> 
#> Overall Statistics
#>                                      
#>                Accuracy : 1          
#>                  95% CI : (0.6637, 1)
#>     No Information Rate : 0.3333     
#>     P-Value [Acc > NIR] : 5.081e-05  
#>                                      
#>                   Kappa : 1          
#>                                      
#>  Mcnemar's Test P-Value : NA         
#> 
#> Statistics by Class:
#> 
#>                      Class: A Class: B Class: C
#> Sensitivity            1.0000   1.0000   1.0000
#> Specificity            1.0000   1.0000   1.0000
#> Pos Pred Value         1.0000   1.0000   1.0000
#> Neg Pred Value         1.0000   1.0000   1.0000
#> Prevalence             0.3333   0.3333   0.3333
#> Detection Rate         0.3333   0.3333   0.3333
#> Detection Prevalence   0.3333   0.3333   0.3333
#> Balanced Accuracy      1.0000   1.0000   1.0000
#> 
#> *************** Map ******************
#> $map
#> class       : SpatRaster
#> size        : 77, 101, 1  (nrow, ncol, nlyr)
#> resolution  : 1, 1  (x, y)
#> extent      : 0, 101, 0, 77  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#> source(s)   : memory
#> name        : class_supervised
#> min value   :                1
#> max value   :                3

## Plots
plot(SC$map, col = colors, legend = FALSE, axes = FALSE, box = FALSE)
legend(1,1, legend = levels(train$class), fill = colors , title = "Classes", 
horiz = TRUE,  bty = "n")

par(olpar) # reset par
```
