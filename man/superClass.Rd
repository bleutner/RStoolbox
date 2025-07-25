% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/superClass.R
\name{superClass}
\alias{superClass}
\title{Supervised Classification}
\usage{
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
}
\arguments{
\item{img}{SpatRaster. Typically remote sensing imagery, which is to be classified.}

\item{trainData}{sf or sp spatial vector data containing the training locations (POINTs,or POLYGONs).}

\item{valData}{Ssf or sp spatial vector data containing the validation locations (POINTs,or POLYGONs) (optional).}

\item{responseCol}{Character or integer giving the column in \code{trainData}, which contains the response variable. Can be omitted, when \code{trainData} has only one column.}

\item{nSamples}{Integer. Number of samples per land cover class. If \code{NULL} all pixels covered by training polygons are used (memory intensive!). Ignored if trainData consists of POINTs.}

\item{nSamplesV}{Integer. Number of validation samples per land cover class. If \code{NULL} all pixels covered by validation polygons are used (memory intensive!). Ignored if valData consists of POINTs.}

\item{polygonBasedCV}{Logical. If \code{TRUE} model tuning during cross-validation is conducted on a per-polygon basis. Use this to deal with overfitting issues. Does not affect training data supplied as SpatialPointsDataFrames.}

\item{trainPartition}{Numeric. Partition (polygon based) of \code{trainData} that goes into the training data set between zero and one. Ignored if \code{valData} is provided.}

\item{model}{Character. Which model to use. See \link[caret]{train} for options. Defaults to randomForest ('rf'). In addition to the standard caret models, a maximum likelihood classification is available via \code{model = 'mlc'}.}

\item{tuneLength}{Integer. Number of levels for each tuning parameter (see \link[caret]{train} for details).}

\item{kfold}{Integer. Number of cross-validation resamples during model tuning.}

\item{sampling}{Character. Describes the type of additional sampling that is conducted after resampling (usually to resolve class imbalances), from caret. Currently supported are \code{up}, \code{down}, \code{smote}, and \code{rose}. Note, that \code{smote} requires the packages \code{themis} and \code{rose} the package \code{ROSE}. Latter is noly for binary classification problems.}

\item{minDist}{Numeric. Minumum distance between training and validation data,
 e.g. \code{minDist=1} clips validation polygons to ensure a minimal distance of one pixel (pixel size according to \code{img}) to the next training polygon. 
Requires all data to carry valid projection information.}

\item{mode}{Character. Model type: 'regression' or 'classification'.}

\item{predict}{Logical. Produce a map (TRUE, default) or only fit and validate the model (FALSE).}

\item{predType}{Character. Type of the final output raster. Either "raw" for class predictions or "prob" for class probabilities. Class probabilities are not available for all classification models (\link[caret]{predict.train}).}

\item{filename}{Path to output file (optional). If \code{NULL}, standard raster handling will apply, i.e. storage either in memory or in the raster temp directory.}

\item{verbose}{Logical. prints progress and statistics during execution}

\item{overwrite}{logical. Overwrite spatial prediction raster if it already exists.}

\item{...}{further arguments to be passed to \code{\link[caret]{train}}}
}
\value{
A superClass object (effectively a list) containing:
\enumerate{
\item $model: the fitted model
\item $modelFit: model fit statistics
\item $training: indexes of samples used for training
\item $validation: list of
    \enumerate{
       \item $performance: performance estimates based on independent validation (confusion matrix etc.)
       \item $validationSamples: actual pixel coordinates plus reference and predicted values used for validation
       \item $validationGeometry: validation polygpns (clipped with mindist to training geometries)
    }
\item $map: the predicted raster
\item $classMapping: a data.frame containing an integer <-> label mapping
}
}
\description{
Supervised classification both for classification and regression mode based on vector training data (points or polygons).
}
\details{
Note that superClass automatically loads the lattice and randomForest package.
SuperClass performs the following steps:

\enumerate{
\item Ensure non-overlap between training and validation data. This is neccesary to avoid biased performance estimates.
 A minimum distance (\code{minDist}) in pixels can be provided to enforce a given distance between training and validation data.

\item Sample training coordinates. If \code{trainData} (and \code{valData} if present) are polygons \code{superClass} will calculate the area per polygon and sample
\code{nSamples} locations per class within these polygons. The number of samples per individual polygon scales with the polygon area, i.e. the bigger the polygon, the more samples.

\item Split training/validation      
If \code{valData} was provided (reccomended) the samples from these polygons will be held-out and not used for model fitting but only for validation. 
If \code{trainPartition} is provided the trainingPolygons will be divided into training polygons and validation polygons.

\item Extract raster data
The predictor values on the sample pixels are extracted from \code{img}

\item Fit the model. Using caret::train on the sampled training data the \code{model} will be fit, 
including parameter tuning (\code{tuneLength}) in \code{kfold} cross-validation. \code{polygonBasedCV=TRUE} will define cross-validation folds based on polygons (reccomended)
otherwise it will be performed on a per-pixel basis.

\item Predict the classes of all pixels in \code{img} based on the final model.

\item Validate the model with the independent validation data.
}
}
\examples{
library(RStoolbox)
library(caret)
library(randomForest)
library(e1071)
library(terra)
train <- readRDS(system.file("external/trainingPoints_rlogo.rds", package="RStoolbox"))

## Plot training data
olpar <- par(no.readonly = TRUE) # back-up par
par(mfrow=c(1,2))
colors <- c("yellow", "green", "deeppink")
plotRGB(rlogo)
plot(train, add = TRUE, col =  colors[train$class], pch = 19)

## Fit classifier (splitting training into 70\\% training data, 30\\% validation data)
SC       <- superClass(rlogo, trainData = train, responseCol = "class",
model = "rf", tuneLength = 1, trainPartition = 0.7)
SC

## Plots
plot(SC$map, col = colors, legend = FALSE, axes = FALSE, box = FALSE)
legend(1,1, legend = levels(train$class), fill = colors , title = "Classes", 
horiz = TRUE,  bty = "n")
par(olpar) # reset par
}
\seealso{
\code{\link[caret]{train}}
}
