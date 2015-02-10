#' Supervised Classification
#' 
#' Supervised classification both for classification and regression mode based on vector training data (points or polygons). 
#' 
#' @param inputRaster Raster* object. Typically remote sensing imagery, which is to be classified.
#' @param trainData SpatialPolygonsDataFrame or SpatialPointsDataFrame containing the training locations.
#' @param valData  SpatialPolygonsDataFrame or SpatialPointsDataFrame containing the validation locations (optional).
#' @param responseCol Character or integer giving the column in \code{trainData}, which contains the response variable. Can be omitted, when \code{trainData} has only one column.
#' @param nSamples Integer. Number of samples per land cover class.
#' @param areaWeightedSampling logical. If \code{TRUE} scales sample size per polygon area. The bigger the polygon the more samples are taken.
#' @param polygonBasedCV Logical. If \code{TRUE} model tuning during cross-validation is conducted on a per-polygon base. Use this to combat overfitting.
#' @param trainPartition numeric. Partition (polygon based) of \code{trainData} that goes into the training data set between zero and one . Ignored if \code{valData} is provided.
#' @param model Character. Which model to use. See \link[caret]{train} for options. Defaults to randomForest ('rf')
#' @param tuneLength Integer. Number of levels for each tuning paramete (see \link[caret]{train} for details).
#' @param kfold Integer. Number of cross-validation resamples during model tuning.
#' @param minDist Numeric. Minumum distance factor between training and validation data, e.g. minDist=1 will clip validation polygons to ensure a minimal distance of one pixel to the next training polygon. Applies onl if trainData and valData overlap or forceBuffer is \code{TRUE}.
#' @param forceBuffer Logical. Forces a buffer distance of width \code{minDist} betwenn training and validation data.
#' @param filename path to output file (optional). If \code{NULL}, standard raster handling will apply, i.e. storage either in memory or in the raster temp directory.
#' @param verbose logical. prints progress and statistics during execution
#' @param predict logical. \code{TRUE} (default) will return a classified map, \code{FALSE} will only train the classifier
#' @param overwrite logical. Overwrite spatial prediction raster if it already exists.
#' @param ... further arguments to be passed to \code{\link[caret]{train}}
#' @note 
#' Validation on a separate (not used for training) set of polygons/points is highly advised. Automatic validation is performed either by specifying 
#' \code{valData} or by specifying \code{trainPartition}, the amount of \code{trainData} which is to be held out for validation. 
#' @return A list containing [[1]] the model, [[2]] the predicted raster and [[3]] the class mapping  
#' @seealso \code{\link[caret]{train}} 
#' @export
#' @examples 
#' \dontshow{
#' library(randomForest)
#' library(e1071)
#' }
#' input <- brick(system.file("external/rlogo.grd", package="raster"))
#' train <- readRDS(system.file("external/training.rds", package="RStoolbox"))
#' 
#' ## Plot training data
#' olpar <- par(no.readonly = TRUE) # back-up par
#' par(mfrow=c(1,2))
#' colors <- c("yellow", "green", "deeppink")
#' plotRGB(input)
#' plot(train, add = TRUE, col =  colors[train$class], pch = 19)
#' 
#' ## Fit classifier (splitting training into 70\% training data, 30\% validation data)
#' SC 	  <- superClass(input, trainData = train, responseCol = "class", 
#' model = "rf", tuneLength = 1, trainPartition = 0.7)
#' SC
#' 
#' ## Plots
#' plot(SC$map, col = colors, legend = FALSE, axes = FALSE, box = FALSE)
#' legend(1,1, legend = levels(train$class), fill = colors , title = "Classes", 
#' horiz = TRUE,  bty = "n")
#' par(olpar) # reset par
superClass <- function(inputRaster, trainData, valData = NULL, responseCol = NULL, nSamples = 100,
        areaWeightedSampling = TRUE, polygonBasedCV = FALSE, trainPartition = NULL,
        model = "rf", tuneLength = 3,  kfold = 5,
        minDist = 2, forceBuffer = FALSE,
        filename = NULL, verbose = FALSE,
        predict = TRUE, overwrite = TRUE, ...) {
    # TODO: check applicability of raster:::.intersectExtent 
    
    ## Object types
    if(!inherits(inputRaster, 'Raster')) stop("inputRaster must be a raster object (RasterLayer,RasterBrick or RasterStack)", call.=FALSE)
    if(inherits(trainData, 'SpatialPolygonsDataFrame')) {
        trainDataType <- "polygons"
    } else {
        if(inherits(trainData, 'SpatialPointsDataFrame')){
            trainDataType <- "points"
        } else {
            stop("traingData must be a SpatialPolygonsDataFrame or a SpatialPointsDataFrame", call.=FALSE)
        }
    } 
    
    ## Attribute column
    if(is.numeric(responseCol)) responseCol <- colnames(trainData@data)[responseCol]
    if(is.null(responseCol)){
        if(ncol(trainData) == 1) {
            responseCol <- 1
            message("You did not specify the responseCol argument. \nSince your trainData only contains one column it is assumed this is it")
        } else {
            stop(paste("Dont't know which column in trainData contains the class attribute. \nPlease specify responseCol as one of: ", paste(colnames(trainData@data),collapse=", ")), call. = FALSE)
        }
    }    
    if(!responseCol %in% colnames(trainData@data))
        stop(paste0("The column ", responseCol, " does not exist in trainData. \nAvailable columns are: ", paste0(colnames(trainData@data),collapse=", ")), call. = FALSE) 
    if(!is.null(valData) && !responseCol %in% colnames(valData@data)) 
        stop(paste0("The column ", responseCol, " does not exist in valData. \nAvailable columns are: ", paste0(colnames(valData@data),collapse=", ")), call. = FALSE) 
    if(!is.null(valData) && !all.equal(class(trainData), class(valData)))
        stop("trainData and valData must be of the same class. Either SpatialPointsDataFrame or SpatialPolygonsDataFrame.")
    
    ## Check projections
    if(!compareCRS(inputRaster, trainData)) 
        stop("Projection of trainData does not match inputRaster")
    
    ## Check overlap of vector and raster data	
    if(!gIntersects(as(extent(inputRaster),"SpatialPolygons"), as(extent(trainData),"SpatialPolygons"))) 
        stop("inputRaster and trainData do not overlap")
    
    ## What's happening? Class or Reg
    mode <- if(is.numeric(trainData[[responseCol]])) "regression" else "classification"
    
    ## Split into training and validation data (polygon basis)
    if(is.null(valData) & !is.null(trainPartition)){
        training  <- createDataPartition(trainData[[responseCol]], p = trainPartition)[[1]] ## this works for polygons as well because every polygon has only one entry in the attribnute table @data
        valData   <- trainData[-training,]
        trainData <- trainData[training,]
    }
    
    ## Check for and deal with overlapping training & validation data                 
    if(forceBuffer || trainDataType == "points" & minDist != 0) dummy <- gBuffer(trainData, width = res(inputRaster)[1]*minDist, byid = TRUE)  
    
    ## We need this cumbersome any(gDisjoint(byid=T)) action to circumvent TopologyExceptions
    if(!is.null(valData) && any(!gDisjoint(trainData, valData, byid =T)) || exists("dummy") &&  any(gIntersects(valData, dummy, byid=T))){
        if(identical(trainData, valData)) stop("trainData is the same as valData")
        nValOrig <- nrow(valData)
        if(trainDataType == "polygons"){
            ## Clip validation data to training data + 2 pixel buffer 
            #dissolve(gUnionCascaded(trainData, trainData[[responseCol]]))        
            inter <- gIntersection(valData, trainData, byid = TRUE) ## again both steps needed to deal with poor poly data potentially arising from manually digitizing training areas
            inter <- gUnionCascaded(inter)
            if(minDist != 0) inter <- gBuffer(inter, width = res(inputRaster)[1] * minDist)
            clip  <- gDifference(valData, inter, byid = TRUE)
            if(is.null(clip)) stop("After clipping valData to trainData+minDist*pix buffer no validation polygons remain. Please provide non-overlapping trainData and valData.")
            classVec <- data.frame(x = valData[[responseCol]][over(clip, valData)])
            valData <- as(clip, "SpatialPolygonsDataFrame")
            valData@data <- classVec 
            colnames(valData@data) <- responseCol
        } else { 
            if(!exists("dummy")) dummy <- trainData 
            inter <- gIntersects(valData, dummy, byid = T)
            inter <- colnames(inter)[which(inter, arr.ind = TRUE)[,2]]
            valData <- valData[!rownames(valData@data) %in% inter,]        
        }     
        if(!forceBuffer) warning("TrainData and valData overlap. Excluded overlapping points or polygon areas.\n" , nrow(valData), " of ", nValOrig, " remain for validation.")
    }   
    
    ## Creade hold out indices on polygon level
    if(polygonBasedCV){
        folds <- createFolds(trainData@data[[responseCol]], k = kfold)
        names(folds) <- NULL
        folds <- melt(folds) 
        foldCol <- "excludeFromFold"
        trainData@data[[foldCol]]<- folds[order(folds$value),"L1"]
    } else {
        foldCol <- NULL
    }
    
    ## Calculate area weighted number of samples per polygon
    ## we'll end up with n > nSamples, but make sure to sample each polygon at least once
    .samplePixels <- function(SHAPE, RASTER, foldCol = NULL){
        if(trainDataType == "polygons"){
            if (areaWeightedSampling){
                if(is.projected(SHAPE)){
                    SHAPE[["area"]] <- gArea(SHAPE, byid = TRUE)
                } else {
                    SHAPE[["area"]] <- areaPolygon(SHAPE)		
                }
            } else {
                SHAPE[["area"]] <- 1
            }
            
            ## Calculate optimal nSamples per class
            SHAPE@data[["order"]] <- 1:nrow(SHAPE) 		
            weights <- ddply(SHAPE@data, .variables = responseCol, .fun = here(mutate), nSamplesClass = ceiling(nSamples * area / sum(area)))
            SHAPE@data <- weights[order(weights$order),]
            
            ## Get random coordinates within polygons
            xy  <- lapply(seq_along(SHAPE), function(i_poly){	
                        pts <- spsample(SHAPE[i_poly, ], type = "random", n = SHAPE@data[i_poly,"nSamplesClass"], iter = 20) 
                    })
            xy <- do.call("rbind", xy)
        } else {
            xy <- SHAPE
        }
        
        ## Extract response and predictors and combine in final training set
        if(verbose) message("Begin sampling training data")
        dataSet <- data.frame(
                if(trainDataType == "polygons") over(x = xy, y = SHAPE)[c(responseCol,foldCol)] else SHAPE[[responseCol]],
                extract(RASTER, xy, cellnumbers = TRUE))
        
        ## Discard duplicate cells
        dubs 	<- duplicated(dataSet[,"cells"])
        dataSet <- dataSet[!dubs,]
        dataSet$cells <- NULL
        colnames(dataSet)[1] <- "response"
        dataSet
    }
    
    dataSet  <- .samplePixels(SHAPE = trainData, RASTER=inputRaster, foldCol=foldCol)
    if(polygonBasedCV) {
        indexOut <- dataSet[[foldCol]]
        dataSet[[foldCol]] <- NULL
    }
    ## Unique classes
    if(mode == "classification"){   
        if(!is.factor(dataSet$response)) dataSet$response <- as.factor(dataSet$response)
        classes 	 <- unique(dataSet$response)
        classMapping <- data.frame(classID = as.numeric(classes), class = levels(classes))
    }
    
    ## Meaningless predictors
    uniqueVals  <- apply(dataSet, 2, function(x){length(unique(x))}) == 1
    if(uniqueVals[1]) stop("Response (responseCol in trainData) contains only one value. Classification doesn't make sense in this case.")
    if(any(uniqueVals)) {
        warning( "Samples from ", paste0(colnames(dataSet)[uniqueVals], collapse = ", "), " contain only one value. The variable will be omitted from model training.")
        dataSet <- dataSet[, !uniqueVals, drop=FALSE]
    }
    
    ## TRAIN ######################### 
    if(verbose) message("Starting to fit model")   
    indexIn <- if(polygonBasedCV) lapply(1:kfold, function(x) which(x != indexOut)) 
    caretModel 	<- train(response ~ ., data = dataSet, method = model, tuneLength = tuneLength, 
            trControl = trainControl(method = "cv", number = kfold, index = indexIn), ...)   
    
    ## PREDICT ######################### 
    progress <- "none"
    if(verbose) { 
        message("Starting spatial predict")
        progress <- "text"
    }
    
    ## Don't know whether we need this, who would be crazy enough to do more than 255 classes...
    modelFit <- getTrainPerf(caretModel)
    dataType <- NULL
    if(mode == "classification") {
        dataType <- if(length(classes) < 255) "INT1U" else "INT2U"
        modelFit <- list(modelFit, confusionMatrix(caretModel, norm = "average"))     
    } 
    
    args <- list(model = caretModel, filename = filename, progress = progress, datatype = dataType, overwrite = overwrite)
    args$filename <- filename ## remove filename from args if is.null(filename) --> standard writeRaster handling applies
    spatPred <- .paraRasterFun(inputRaster, rasterFun=raster::predict, args = args)
    names(spatPred) <- responseCol
    
    ## VALIDATION ########################
    if(!is.null(valData)){
        valiSet <- .samplePixels(valData, spatPred)
        colnames(valiSet) <- c("reference", "prediction")
        if(mode == "classification"){
            if(!is.factor(valiSet$reference)) valiSet$reference <- factor(valiSet$reference, levels = levels(classes))
            valiSet$prediction <- classes[valiSet$prediction]
            validation <- confusionMatrix(data = valiSet$prediction, reference = valiSet$reference)              
        } else {
            valiSet$residuals <- valiSet$reference - valiSet$predicted
            validation <-  data.frame(rmse = RMSE(valiSet$prediction, valiSet$reference), rsquared = R2(valiSet$prediction, valiSet$reference))   
        }
        validation <- list(performance = validation, validationSet = valiSet)
    } else {
        validation <- "No independent validation was performed!"
    }
    
    ## Print summary stats
    if(verbose){
        message(paste0(paste0(rep("*",20), collapse = "")," Model summary " ,paste0(rep("*",20), collapse = "")))
        print(caretModel)
        print(modelFit)
        message(paste0(paste0(rep("*",20), collapse = "")," Validation summary " ,paste0(rep("*",20), collapse = "")))
        print(validation[[1]])
    }
    
    out <- list(model = caretModel, modelFit = modelFit, validation = validation, map = spatPred)
    if(exists("training")) out <- c(out, trainingPartitionIndices = training)
    if(mode == "classification") out <- c(out, classMapping = classMapping) 
    structure(out, class = "superClass")
}

#' @method print superClass
#' @export 
print.superClass <- function(x,...){
    cat("superClass results\n")
    cat("************ Validation **************\n")
    cat("$validation\n")   
    print(x$validation[[1]])
    cat("\n*************** Map ******************\n")
    cat("$map\n")
    show(x$map)
}

