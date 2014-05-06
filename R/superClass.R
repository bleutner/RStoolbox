#' Supervised Classification
#' 
#' @param inputRaster Raster* object. Typically remote sensing imagery, which is to be classified.
#' @param trainingData SpatialPolygonsDataFrame containing the training data used to train the classifier.  
#' @param classAttributes character giving the column in \code{trainingData}, which contains the class attribute. Can be omitted, when \code{trainingData} has only one column.
#' @param nSamples number of samples per land cover class
#' @param filename path to output file (optional). If \code{NULL}, standard raster handling will apply, i.e. storage either in memory or in the raster temp directory.
#' @param maskRaster Raster layer containing a binary mask to exclude areas from prediction.
#' @param verbose logical. prints progress, statistics and graphics during execution
#' @param predict logical. \code{TRUE} (default) will return a classified map, \code{FALSE} will only train the classifier
#' @param ... further arguments to be passed to randomForest
#' @return A list containing [[1]] the model, [[2]] the predicted raster and [[3]] the class mapping  
#' @seealso \code{\link{randomForest}} 
#' @export
superClass <- function(inputRaster, trainingData, classAttributes = NULL, nSamples = 100, filename = NULL, maskRaster = NULL, verbose = FALSE, predict = TRUE, overwrite = TRUE, ...) {
	# TODO: point vector data
	# TODO: enable regression mode
	# TODO: cross-validation
	# TODO: make classifier modular
	# TODO: add examples
	# DISCUSS: demo data
	
	## Filetypes
	if(!inherits(inputRaster, 'Raster')) stop("inputRaster must be a raster object (RasterLayer,RasterBrick or RasterStack)", call.=FALSE)
	if(!inherits(trainingData, 'SpatialPolygonsDataFrame')) stop("traingData must be a SpatialPolygonsDataFrame", call.=FALSE)
	
	## Attribute column
	if(is.null(classAttributes)){
		if(ncol(trainingData) == 1) {
			classAttributes <- 1
			message("You did not specify the classAttributes column. \nSince your trainingData only contains one column we assume this is it")
		} else {
			stop(paste("Dont't know which column in trainingData contains the class attribute. \nPlease specify classAttributes as one of: ", paste(colnames(trainingData@data),collapse=", ")), call. = FALSE)
		}
	} 
	if(!classAttributes %in% colnames(trainingData@data)) 
		stop(paste0("The column ", classAttributes, " does not exist in trainingData. \nAvailable columns are: ", colnames(trainingData@data,collapse=", ")), call. = FALSE) 
		
	## Check projections
	if(!compareCRS(inputRaster, trainingData)) 
		stop("Projection of trainingData does not match inputRaster")
		## DISCUSS: Should we do a spTransform of vector data here, or require proper projection from the user?
	
	## Check overlap of vector and raster data	
	if(!gIntersects(as(extent(inputRaster),"SpatialPolygons"), as(extent(trainingData),"SpatialPolygons"))) 
		stop("inputRaster and trainingData do not overlap")
	
	## Calculate area weighted number of samples per polygon
	## this way we'll end up with n > nSamples, but make sure to sample each polygon at least once
	if(is.projected(trainingData)){
		trainingData[["area"]] <- gArea(trainingData, byid = TRUE)
	} else {
		trainingData[["area"]] <- areaPolygon(trainingData)		
	}
	
	## Calculate optimal nSamples per class
	trainingData@data[["order"]] <- 1:nrow(trainingData) 		
	weights <- ddply(trainingData@data, .variables = classAttributes, .fun = here(mutate), nSamplesClass = ceiling(nSamples * area / sum(area)))
	trainingData@data <- weights[order(weights$order),]
		
	## Get random coordinates within polygons
	xy  <- lapply(seq_along(trainingData), function(i_poly){	
				pts <- spsample(trainingData[i_poly, ], type = "random", n = trainingData@data[i_poly,"nSamplesClass"], iter = 20) 
			})
	xy <- do.call("rbind", xy)
	
	### Display, verbose only
	if(verbose) {
		plot(inputRaster,1)
		plot(trainingData, add = T)
		points(xy, pch = 3, cex = 0.5)
	}	
	
	## Extract response and predictors and combine in final training set
	if(verbose) print("Begin extract")
	dataSet <- data.frame(
			response = as.factor(over(x = xy, y = trainingData)[[classAttributes]]),
			extract(inputRaster, xy, cellnumbers = TRUE))
	
	## Discard duplicate cells
	dataSet <- dataSet[!duplicated(dataSet[,"cells"]),]
	dataSet <- dataSet[,colnames(dataSet) != "cells"]
	
	## Unique classes
	classes <- unique(trainingData[[classAttributes]])
	classMapping <- data.frame(classID = as.numeric(classes), class = levels(classes))
	
	## TRAIN ######################### 
	if(verbose) print("Starting to calculate random forest model") 
	model <- randomForest(response ~ . , data = dataSet, na.action = na.omit, confusion = TRUE, ...)		
	
	## PREDICT ######################### 
	progress <- "none"
	if(verbose) { print("Starting spatial predict")
		progress <- "text"
	}
	 
	## Don't know whether we need this, who would be crazy enough to do more than 255 classes...
	ifelse(length(classes) < 255, dataType <- "INT1U",  dataType <- "INT2U")
	
	if(is.null(filename)){
		spatPred <- predict(inputRaster, model, progress = progress, dataType = dataType, overwrite = overwrite)
	} else {
		spatPred <- predict(inputRaster, model, filename = filename, progress = progress, dataType = dataType, overwrite = overwrite)
	}
	 
	## Print summary stats
	if(verbose)
		print(paste0(paste0(rep("*",20), collapse = "")," Classification summary " ,paste0(rep("*",20), collapse = "")))
		## Samples total
		## Samples per class
		print(model)
	## TODO: calculate users,producer's accuracies and kappas
	
	## DISCUSS: should we return sample points as well?
	return(list(model = model, map = spatPred, classMapping = classMapping)) 
	
}

