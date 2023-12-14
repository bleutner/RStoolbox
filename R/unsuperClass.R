#' Unsupervised Classification
#' 
#' Unsupervised clustering of SpatRaster data using kmeans clustering
#' 
#' @param img SpatRaster.
#' @param nSamples Integer. Number of random samples to draw to fit cluster map. Only relevant if clusterMap = TRUE.
#' @param nClasses Integer. Number of classes.
#' @param nStarts  Integer. Number of random starts for kmeans algorithm.
#' @param nIter Integer. Maximal number of iterations allowed.
#' @param norm Logical. If \code{TRUE} will normalize img first using \link{normImage}. Normalizing is beneficial if your predictors have different scales.
#' @param clusterMap Logical. Fit kmeans model to a random subset of the img (see Details).
#' @param algorithm Character. \link[stats]{kmeans} algorithm. One of c("Hartigan-Wong", "Lloyd", "MacQueen")
#' @param output Character. Either 'classes' (kmeans class; default) or 'distances' (euclidean distance to each cluster center).
#' @param ... further arguments to be passed to \link[terra]{writeRaster}, e.g. filename
#' @details 
#' Clustering is done using \code{\link[stats]{kmeans}}. This can be done for all pixels of the image (\code{clusterMap=FALSE}), however this can be slow and is
#' not memory safe. Therefore if you have large raster data (> memory), as is typically the case with remote sensing imagery it is advisable to choose clusterMap=TRUE (the default).
#' This means that a kmeans cluster model is calculated based on a random subset of pixels (\code{nSamples}). Then the distance of *all* pixels to the cluster centers 
#' is calculated in a stepwise fashion using \code{\link[raster]{predict}}. Class assignment is based on minimum euclidean distance to the cluster centers.   
#' 
#' The solution of the kmeans algorithm often depends on the initial configuration of class centers which is chosen randomly. 
#' Therefore, kmeans is usually run with multiple random starting configurations in order to find a convergent solution from different starting configurations.
#' The \code{nStarts} argument allows to specify how many random starts are conducted.   
#' @return 
#' Returns an RStoolbox::unsuperClass object, which is a list containing the kmeans model ($model) and the raster map ($map).
#' For output = "classes", $map contains a SpatRaster with discrete classes (kmeans clusters); for output = "distances" $map contains
#' a SpatRaster, with `nClasses` layers, where each layer maps the euclidean distance to the corresponding class centroid.
#'
#' @export
#' @examples 
#' library(terra)
#' input <- rlogo_rs
#' 
#' ## Plot 
#' olpar <- par(no.readonly = TRUE) # back-up par
#' par(mfrow=c(1,2))
#' plotRGB(input)
#' 
#' ## Run classification
#' set.seed(25)
#' unC <- unsuperClass(input, nSamples = 100, nClasses = 5, nStarts = 5)
#' unC
#' 
#' ## Plots
#' colors <- rainbow(5)
#' plot(unC$map, col = colors, legend = FALSE, axes = FALSE, box = FALSE)
#' legend(1,1, legend = paste0("C",1:5), fill = colors,
#'        title = "Classes", horiz = TRUE,  bty = "n")
#' 
#' ## Return the distance of each pixel to each class centroid
#' unC <- unsuperClass(input, nSamples = 100, nClasses = 3, output = "distances")
#' unC
#' \dontrun{ggR(unC$map, 1:3, geom_raster = TRUE)}
#' 
#' par(olpar) # reset par
unsuperClass <- function(img, nSamples = 10000, nClasses = 5, nStarts = 25, nIter = 100, norm = FALSE, 
                         clusterMap = TRUE, algorithm = "Hartigan-Wong", output  = "classes", ...){      
  ## TODO: check outermost prediction (cpp)
  img <- .toTerra(img)
  if(atMax <- nSamples > ncell(img)) nSamples <- ncell(img)
  if(norm) img <- terra::scale(img, TRUE, TRUE)
  
  if(!output[1] %in% c("classes", "distances")) {
    stop("`output` must be either 'classes' or 'distances'")
  }
  returnDistances <- FALSE
  if(output == "distances") {
    returnDistances <- TRUE
  }
  
  FULL <- !clusterMap | atMax && .canProcInMem(img, n = 4)
  
  if(FULL){
    if(!inMemory(img)).vMessage("Loading full raster into memory")
    trainData <- img[]
    complete  <- complete.cases(trainData)
    trainData <- trainData[complete,]
  } else {
    if(!clusterMap) warning("Raster size is > memory. Resetting clusterMap to TRUE")
    .vMessage("Starting random sampling")
    trainData <- .iterativeRandomSample(img, nSamples, xy = FALSE)
  }
  
  .vMessage("Starting kmeans fitting")
  model     <- tryCatch(kmeans(trainData, centers = nClasses, nstart = nStarts, iter.max = nIter, algorithm = algorithm))
  if (!is.null(model$ifault)) {
    if(model$ifault == 4 && algorithm == "Hartigan-Wong") {
      warning("The Harian-Wong algorithm doesn't converge properly.", 
              "\nConsider setting algorithm to 'Lloyd' or 'MacQueen' and/or increase nStarts", call. = FALSE) 
    } else if (model$ifault==2) {
      warning("The kmeans algorithm did not converge. Try increasing nIter.", call. = FALSE)
    }
  }
  
  .vMessage("Starting spatial prediction")
  if(FULL && !returnDistances){
    out           <- rast(img, vals = NA, nlyrs = 1)
    out[complete] <- model$cluster      
    names(out)    <- "class"
    if("filename" %in% list(...)) out <- terra::writeRaster(out, ...)
  } else {
    out   <- app(img, fun = function(x, kmeans= model){
      predKmeansCpp(x, centers=kmeans$centers, returnDistances)}, ...)
    names(out) <- if(returnDistances) paste0("dist_c", 1:nClasses) else "class"
  }
  
  model$cluster <- NULL
  structure(list(call = match.call(), model = model, map = out), class = c("unsuperClass", "RStoolbox"))
}




#' Predict a raster map based on a unsuperClass model fit.
#' 
#' applies a kmeans cluster model to all pixels of a raster.
#' Useful if you want to apply a kmeans model of scene A to scene B.
#' 
#' @method predict unsuperClass
#' @param object unsuperClass object
#' @param img Raster object. Layernames must correspond to layernames used to train the superClass model, i.e. layernames in the original raster image.
#' @param output Character. Either 'classes' (kmeans class; default) or 'distances' (euclidean distance to each cluster center).
#' @param ... further arguments to be passed to \link[raster]{writeRaster}, e.g. filename
#' @export 
#' @examples 
#' ## Load training data
#' 
#' ## Perform unsupervised classification
#' uc  <- unsuperClass(rlogo_rs, nClasses = 10)
#' 
#' ## Apply the model to another raster
#' map <- predict(uc, rlogo_rs)
predict.unsuperClass <- function(object, img,  output  = "classes", ...){
  img <- .toTerra(img)
  stopifnot(inherits(object, c("RStoolbox", "unsuperClass")))
  if(!output[1] %in% c("classes", "distances")) {
    stop("`output` must be either 'classes' or 'distances'")
  }
  returnDistances <- FALSE
  if(output == "distances") {
    returnDistances <- TRUE
  }
  
  out <- app(img, fun = function(x, kmeans= object$model){
    predKmeansCpp(x, centers=kmeans$centers, returnDistances)}, ...)
  names(out) <- if(returnDistances) paste0("dist_c", 1:nlyr(out)) else "class"
  return(out)
}




#' @method print unsuperClass
#' @export 
print.unsuperClass <- function(x, ...){
  cat("unsuperClass results\n")    
  cat ("\n*************** Model ******************\n")
  cat ("$model\n")
  cat("K-means clustering with ", length(x$model$size), " clusters of sizes ",
      paste(x$model$size, collapse = ", "), "\n", sep = "")
  cat("\nCluster centroids:\n")
  print(x$model$centers, ...)
  cat("\nWithin cluster sum of squares by cluster:\n")
  print(x$model$withinss, ...)
  ratio <- sprintf(" (between_SS / total_SS = %5.1f %%)\n",
                   100 * x$model$betweenss/x$model$totss)
  if(!is.null(x$model$ifault) && x$model$ifault == 2L)
    cat("Warning: did *not* converge in specified number of iterations\n")
  cat("\n*************** Map ******************\n")
  cat("$map\n")
  show(x$map)
}


