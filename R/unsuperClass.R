#' Unsupervised Classification
#' 
#' Unsupervised clustering of Raster* data using kmeans clustering
#' 
#' Clustering is done using stats::kmeans. This can be done for all pixels of the image (clusterMap=FALSE), however this can be slow and is
#' not memory safe. Therefore if you have large raster data (> memory), as is typically the case with remote sensing imagery it is adviseable to choose clusterMap=TRUE (the default).
#' This means that a kmeans cluster map is calculated based on a random subset of pixels (nSamples). Then the distance of *all* pixels to the cluster centers 
#' is calculated in a stepwise fashion using raster::predict. Class assignment is based on minimum euclidean distance to the cluster centers.   
#' 
#' @param img Raster* object. 
#' @param nSamples integer. Number of random samples to draw to fit cluster map. Only relevant if clusterMap = FALSE.
#' @param nClasses integer. Number of classes.
#' @param nStarts  integer. Number of random starts for kmeans algorithm.
#' @param nIter integer. Maximal number of iterations allowed.
#' @param norm Logical. If \code{TRUE} will normalize img first using \link{normImage}. Normalizing is beneficial if your predictors have different scales.
#' @param clusterMap logical. Fit kmeans model to a random subset of the img (see Details).
#' @param algorithm character. \link[stats]{kmeans} algorithm. One of c("Hartigan-Wong", "Lloyd", "MacQueen")
#' @param ... further arguments to be passed to \link[raster]{writeRaster}, e.g. filename
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
#' par(olpar) # reset par
unsuperClass <- function(img, nSamples = 10000, nClasses = 5, nStarts = 25, nIter = 100, norm = FALSE, clusterMap = TRUE, algorithm = "Hartigan-Wong", ...){      
	if(atMax <- nSamples > ncell(img)) nSamples <- ncell(img)
	wrArgs <- list(...)
	if(norm) img <- normImage(img)
	if(!clusterMap | atMax && canProcessInMemory(img, n = 4)){
		.vMessage("Load full raster into memory")
		trainData <- img[]
		complete  <- complete.cases(trainData)
		.vMessage("Starting kmeans fitting")
		model     <- kmeans(trainData[complete,], centers = nClasses, iter.max = nIter, nstart = nStarts, algorithm = algorithm)
		out   	  <- raster(img)
		out[]     <- NA
		out[complete] <- model$cluster      
		if("filename" %in% names(wrArgs)) out <- writeRaster(out, ...)
	} else {
		if(!clusterMap) warning("Raster is > memory. Resetting clusterMap to TRUE")
		.vMessage("Starting random sampling")
		trainData <- sampleRandom(img, size = nSamples, na.rm = TRUE)
		.vMessage("Starting kmeans fitting")
		model     <- tryCatch(kmeans(trainData, centers = nClasses, nstart = nStarts, iter.max = nIter, algorithm = algorithm))
        if (model$ifault==4) { warning("The Harian-Wong algorithm doesn't converge properly. Consider setting algorithm to 'Lloyd' or 'MacQueen'") }
        .vMessage("Starting spatial prediction")
		out 	  <- .paraRasterFun(img, rasterFun=raster::calc, args = list(fun=function(x, kmeans=force(model)){
							predKmeansCpp(x, centers=kmeans$centers)}, forcefun=TRUE), wrArgs = wrArgs)
	}
	structure(list(call = match.call(), model = model, map = out), class = c("unsuperClass", "RStoolbox"))
}

	
		
#' @method print unsuperClass
#' @export 
		print.unsuperClass <- function(x, ...){
	cat("unsuperClass results\n")    
	cat("\n*************** Map ******************\n")
	cat("$map\n")
	show(x$map)
}


