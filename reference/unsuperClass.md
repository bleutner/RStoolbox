# Unsupervised Classification

Unsupervised clustering of SpatRaster data using kmeans clustering

## Usage

``` r
unsuperClass(
  img,
  nSamples = 10000,
  nClasses = 5,
  nStarts = 25,
  nIter = 100,
  norm = FALSE,
  clusterMap = TRUE,
  algorithm = "Hartigan-Wong",
  output = "classes",
  ...
)
```

## Arguments

- img:

  SpatRaster.

- nSamples:

  Integer. Number of random samples to draw to fit cluster map. Only
  relevant if clusterMap = TRUE.

- nClasses:

  Integer. Number of classes.

- nStarts:

  Integer. Number of random starts for kmeans algorithm.

- nIter:

  Integer. Maximal number of iterations allowed.

- norm:

  Logical. If `TRUE` will normalize img first using
  [normImage](https://bleutner.github.io/RStoolbox/reference/normImage.md).
  Normalizing is beneficial if your predictors have different scales.

- clusterMap:

  Logical. Fit kmeans model to a random subset of the img (see Details).

- algorithm:

  Character. [kmeans](https://rdrr.io/r/stats/kmeans.html) algorithm.
  One of c("Hartigan-Wong", "Lloyd", "MacQueen")

- output:

  Character. Either 'classes' (kmeans class; default) or 'distances'
  (euclidean distance to each cluster center).

- ...:

  further arguments to be passed to
  [writeRaster](https://rspatial.github.io/terra/reference/writeRaster.html),
  e.g. filename

## Value

Returns an RStoolbox::unsuperClass object, which is a list containing
the kmeans model (\$model) and the raster map (\$map). For output =
"classes", \$map contains a SpatRaster with discrete classes (kmeans
clusters); for output = "distances" \$map contains a SpatRaster, with
\`nClasses\` layers, where each layer maps the euclidean distance to the
corresponding class centroid.

## Details

Clustering is done using
[`kmeans`](https://rdrr.io/r/stats/kmeans.html). This can be done for
all pixels of the image (`clusterMap=FALSE`), however this can be slow
and is not memory safe. Therefore if you have large raster data (\>
memory), as is typically the case with remote sensing imagery it is
advisable to choose clusterMap=TRUE (the default). This means that a
kmeans cluster model is calculated based on a random subset of pixels
(`nSamples`). Then the distance of \*all\* pixels to the cluster centers
is calculated in a stepwise fashion using
[`predict`](https://rspatial.github.io/terra/reference/predict.html).
Class assignment is based on minimum euclidean distance to the cluster
centers.

The solution of the kmeans algorithm often depends on the initial
configuration of class centers which is chosen randomly. Therefore,
kmeans is usually run with multiple random starting configurations in
order to find a convergent solution from different starting
configurations. The `nStarts` argument allows to specify how many random
starts are conducted.

## Examples

``` r
if (FALSE) { # \dontrun{
library(terra)
input <- rlogo

## Plot 
olpar <- par(no.readonly = TRUE) # back-up par
par(mfrow=c(1,2))
plotRGB(input)

## Run classification
set.seed(25)
unC <- unsuperClass(input, nSamples = 100, nClasses = 5, nStarts = 5)
unC

## Plots
colors <- rainbow(5)
plot(unC$map, col = colors, legend = FALSE, axes = FALSE, box = FALSE)
legend(1,1, legend = paste0("C",1:5), fill = colors, title = "Classes", horiz = TRUE,  bty = "n")

## Return the distance of each pixel to each class centroid
unC <- unsuperClass(input, nSamples = 100, nClasses = 3, output = "distances")
unC

ggR(unC$map, 1:3, geom_raster = TRUE)

par(olpar) # reset par
} # }
```
