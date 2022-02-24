#' Unsupervised Classification
#' 
#' Unsupervised clustering of Raster* data using kmeans clustering
#' 
#' @param img Raster* object. 
#' @param nSamples Integer. Number of random samples to draw to fit cluster map. Only relevant if clusterMap = TRUE.
#' @param nClasses Integer. Number of classes.
#' @param nStarts  Integer. Number of random starts for kmeans algorithm.
#' @param nIter Integer. Maximal number of iterations allowed.
#' @param norm Logical. If \code{TRUE} will normalize img first using \link{normImage}. Normalizing is beneficial if your predictors have different scales.
#' @param clusterMap Logical. Fit kmeans model to a random subset of the img (see Details).
#' @param algorithm Character. \link[stats]{kmeans} algorithm. One of c("Hartigan-Wong", "Lloyd", "MacQueen")
#' @param output Character. Either 'classes' (kmeans class; default) or 'distances' (euclidean distance to each cluster center).
#' @param ... further arguments to be passed to \link[raster]{writeRaster}, e.g. filename
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
#' For output = "classes", $map contains a RasterLayer with discrete classes (kmeans clusters); for output = "distances" $map contains 
#' a RasterBrick, with `nClasses` layers, where each layer maps the euclidean distance to the corresponding class centroid.
#'
#' @export
#' @examples 
#' library(raster)
#' input <- brick(system.file("external/rlogo.grd", package="raster"))
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
#' ggR(unC, 1:3, geom_raster = TRUE)
#' 
#' par(olpar) # reset par
unsuperClass <- function(img, nSamples = 10000, nClasses = 5, nStarts = 25, nIter = 100, norm = FALSE, 
                         clusterMap = TRUE, algorithm = "Hartigan-Wong", output  = "classes", ...){      
  ## TODO: check outermost prediction (cpp)
   img <- .toRaster(img)
  if(atMax <- nSamples > ncell(img)) nSamples <- ncell(img)
  wrArgs <- list(...)
  if(norm) img <- normImage(img)
  
  if(!output[1] %in% c("classes", "distances")) {
    stop("`output` must be either 'classes' or 'distances'")
  }
  returnDistances <- FALSE
  if(output == "distances") {
    returnDistances <- TRUE
  }
  
  FULL <- !clusterMap | atMax && canProcessInMemory(img, n = 4)
  
  if(FULL){
    if(!inMemory(img)).vMessage("Loading full raster into memory")
    trainData <- img[]
    complete  <- complete.cases(trainData)
    trainData <- trainData[complete,]
  } else {
    if(!clusterMap) warning("Raster size is > memory. Resetting clusterMap to TRUE")
    .vMessage("Starting random sampling")
    trainData <- sampleRandom(img, size = nSamples, na.rm = TRUE)
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
    out       <- raster(img)
    out[]     <- NA
    out[complete] <- model$cluster      
    names(out) <- "class"
    if("filename" %in% names(wrArgs)) out <- writeRaster(out, ...)
  } else {
    out       <- .paraRasterFun(img, rasterFun=raster::calc, args = list(fun=function(x, kmeans=force(model)){
      if(!is.matrix(x)) x <- as.matrix(x)
      predKmeansCpp(x, centers=kmeans$centers, returnDistances)},
      forcefun=TRUE), wrArgs = wrArgs)
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
#' data(rlogo)
#' 
#' ## Perform unsupervised classification
#' uc  <- unsuperClass(rlogo, nClasses = 10)
#' 
#' ## Apply the model to another raster
#' map <- predict(uc, rlogo)
predict.unsuperClass <- function(object, img,  output  = "classes", ...){
  stopifnot(inherits(object, c("RStoolbox", "unsuperClass")))
  if(!output[1] %in% c("classes", "distances")) {
    stop("`output` must be either 'classes' or 'distances'")
  }
  returnDistances <- FALSE
  if(output == "distances") {
    returnDistances <- TRUE
  }
  
  model <- object$model
  wrArgs <- list(...)
  out   <- .paraRasterFun(img, rasterFun=raster::calc, args = list(fun=function(x, kmeans=force(model)){
    if(!is.matrix(x)) x <- as.matrix(x)
    predKmeansCpp(x, centers=kmeans$centers, returnDistances)}, forcefun=TRUE), wrArgs = wrArgs)
  
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


