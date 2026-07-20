# Predict a raster map based on a unsuperClass model fit.

applies a kmeans cluster model to all pixels of a raster. Useful if you
want to apply a kmeans model of scene A to scene B.

## Usage

``` r
# S3 method for class 'unsuperClass'
predict(object, img, output = "classes", ...)
```

## Arguments

- object:

  unsuperClass object

- img:

  Raster object. Layernames must correspond to layernames used to train
  the superClass model, i.e. layernames in the original raster image.

- output:

  Character. Either 'classes' (kmeans class; default) or 'distances'
  (euclidean distance to each cluster center).

- ...:

  further arguments to be passed to
  [writeRaster](https://rspatial.github.io/terra/reference/writeRaster.html),
  e.g. filename

## Value

Returns a raster with the K-means distances base on your object passed
in the arguments.

## Examples

``` r
## Load training data

## Perform unsupervised classification
uc  <- unsuperClass(rlogo, nClasses = 10)

## Apply the model to another raster
map <- predict(uc, rlogo)
```
