#' Supervised Classification
#' 
#' Supervised classification both for classification and regression mode based on vector training data (points or polygons). 
#' 
#' @param img Raster* object. Typically remote sensing imagery, which is to be classified.
#' @param trainData SpatialPolygonsDataFrame or SpatialPointsDataFrame containing the training locations.
#' @param valData  SpatialPolygonsDataFrame or SpatialPointsDataFrame containing the validation locations (optional).
#' @param responseCol Character or integer giving the column in \code{trainData}, which contains the response variable. Can be omitted, when \code{trainData} has only one column.
#' @param nSamples Integer. Number of samples per land cover class.
#' @param polygonBasedCV Logical. If \code{TRUE} model tuning during cross-validation is conducted on a per-polygon basis. Use this to deal with overfitting issues. Does not affect training data supplied as SpatialPointsDataFrames.
#' @param trainPartition Numeric. Partition (polygon based) of \code{trainData} that goes into the training data set between zero and one. Ignored if \code{valData} is provided.
#' @param model Character. Which model to use. See \link[caret]{train} for options. Defaults to randomForest ('rf'). In addition to the standard caret models, a maximum likelihood classification is available via \code{model = 'mlc'}. 
#' @param tuneLength Integer. Number of levels for each tuning parameter (see \link[caret]{train} for details).
#' @param kfold Integer. Number of cross-validation resamples during model tuning.
#' @param minDist Numeric. Minumum distance factor between training and validation data, e.g. minDist=1 will clip validation polygons to ensure a minimal distance of one pixel to the next training polygon. Applies onl if trainData and valData overlap.
#' @param mode Character. Model type: 'regression' or 'classification'. 
#' @param predict Logical. Produce a map (TRUE, default) or only fit and validate the model (FALSE).
#' @param predType Character. Type of the final output raster. Either "raw" for class predictions or "prob" for class probabilities. Class probabilities are not available for all classification models (\link[caret]{predict.train}). 
#' @param filename Path to output file (optional). If \code{NULL}, standard raster handling will apply, i.e. storage either in memory or in the raster temp directory. 
#' @param verbose Logical. prints progress and statistics during execution
#' @param overwrite logical. Overwrite spatial prediction raster if it already exists.
#' @param ... further arguments to be passed to \code{\link[caret]{train}}
#' @details 
#' SuperClass performs the following steps:
#' 
#' \enumerate{
#' \item Ensure non-overlap between training and validation data. This is neccesary to avoid biased performance estimates.
#'  A minimum distance (\code{minDist}) in pixels can be provided to enforce a given distance between training and validation data.
#' 
#' \item Sample training coordinates. If \code{trainData} (and \code{valData} if present) are SpatialPolygonsDataFrames \code{superClass} will calculate the area per polygon and sample
#' \code{nSamples} locations per class within these polygons. The number of samples per individual polygon scales with the polygon area, i.e. the bigger the polygon, the more samples.
#' 
#' \item Split training/validation	  
#' If \code{valData} was provided (reccomended) the samples from these polygons will be held-out and not used for model fitting but only for validation. 
#' If \code{trainPartition} is provided the trainingPolygons will be divided into training polygons and validation polygons.
#' 
#' \item Extract raster data
#' The predictor values on the sample pixels are extracted from \code{img}
#' 
#' \item Fit the model. Using caret::train on the sampled training data the \code{model} will be fit, 
#' including parameter tuning (\code{tuneLength}) in \code{kfold} cross-validation. \code{polygonBasedCV=TRUE} will define cross-validation folds based on polygons (reccomended)
#' otherwise it will be performed on a per-pixel basis.
#'
#' \item Predict the classes of all pixels in \code{img} based on the final model.
#' 
#' \item Validate the model with the independent validation data.
#' }
#' 
#' 
#' @return A list containing [[1]] the model, [[2]] the predicted raster and [[3]] the class mapping  
#' @seealso \code{\link[caret]{train}} 
#' @export
#' @examples 
#' library(caret)
#' library(randomForest)
#' library(e1071)
#' library(raster)
#' data(rlogo)
#' train <- readRDS(system.file("external/trainingPoints.rds", package="RStoolbox"))
#' 
#' ## Plot training data
#' olpar <- par(no.readonly = TRUE) # back-up par
#' par(mfrow=c(1,2))
#' colors <- c("yellow", "green", "deeppink")
#' plotRGB(rlogo)
#' plot(train, add = TRUE, col =  colors[train$class], pch = 19)
#' 
#' ## Fit classifier (splitting training into 70\% training data, 30\% validation data)
#' SC 	  <- superClass(rlogo, trainData = train, responseCol = "class", 
#' model = "rf", tuneLength = 1, trainPartition = 0.7)
#' SC
#' 
#' ## Plots
#' plot(SC$map, col = colors, legend = FALSE, axes = FALSE, box = FALSE)
#' legend(1,1, legend = levels(train$class), fill = colors , title = "Classes", 
#' horiz = TRUE,  bty = "n")
#' par(olpar) # reset par
superClass <- function(img, trainData, valData = NULL, responseCol = NULL,
        nSamples = 1000, polygonBasedCV = FALSE, trainPartition = NULL,
        model = "rf", tuneLength = 3,  kfold = 5,
        minDist = 2,  mode = "classification", predict = TRUE, predType = "raw",
        filename = NULL, verbose,
        overwrite = TRUE, ...) {
    # TODO: check applicability of raster:::.intersectExtent 
    # TODO: check for empty factor levels
    # TODO: consider splitting large polygons if there are few polygons in total
    seeds <- as.list(sample.int(kfold*tuneLength+2))
    if(!missing("verbose")) .initVerbose(verbose)
    verbose <- getOption("RStoolbox.verbose")
    ## Object types
    if(!inherits(img, 'Raster')) stop("img must be a raster object (RasterLayer,RasterBrick or RasterStack)", call.=FALSE)
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
            .vMessage("You did not specify the responseCol argument. \nSince your trainData only contains one column it is assumed this is it")
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
    if(any(!mode %in% c("regression", "classification"))) 
        stop("unknown mode. must be 'regression' or 'classification'")
    
    ## Check projections
    if(!compareCRS(img, trainData)) 
        stop("Projection of trainData does not match img")
    
    ## Check overlap of vector and raster data	
    if(!gIntersects(as(extent(img),"SpatialPolygons"), as(extent(trainData),"SpatialPolygons"))) 
        stop("img and trainData do not overlap")
    
    ## Make sure classification is run with factors
    if(mode == "classification" && is.numeric(trainData[[responseCol]])) {
        trainData[[responseCol]] <- as.factor(trainData[[responseCol]])       
    } 
    
    ## Sanitize arguments (polygonBasedCV is only relevant for polygons)
    if(inherits(trainData, "SpatialPointsDataFrame") & polygonBasedCV) polygonBasedCV <- FALSE
    
    ## Spit ellipsis into caret::trainControl and raster::writeRaster
#    frmls_train <- names(formals(raster::writeRaster))
#    args  <- c(list(...), method = method)
#    args_trainControl  <- args[names(args) %in% frmls_train]
#    args_writeRaster   <- args[!names(args) %in% frmls_train]
#    args_writeRaster$filename <- if(length(classes) == 1) filename else NULL ## write raster here already during predict if only one layer is output
#    
    
    ## Split into training and validation data (polygon basis)
    if(is.null(valData) & !is.null(trainPartition)){
        training  <- createDataPartition(trainData[[responseCol]], p = trainPartition)[[1]] ## this works for polygons as well because every polygon has only one entry in the attribnute table @data
        if(length(training) == nrow(trainData)) stop(paste0("There are not enough polygons to split into training and validation partitions. \n  You could either ",
                            "\n   * provide more (often smaller) polygons instead of few large ones (recommended)",
                            "\n   * provide pre-defined validation polygons via the valData argument",
                            "\n   * decrease trainPartition",
                            "\n   * run without independent validation."))
        if(mode == "classification"){
            valVal <- trainData[[responseCol]][-training]
            valDiff <- setdiff(trainData[[responseCol]], valVal)
            if(length(valDiff)) stop(paste0("The independent validation partition does not encompass all classes.",
                                "\n   Missing classes: ", paste(valDiff, collapse = ", "), 
                                "\n   You could either ",
                                "\n    * provide more (often smaller) polygons for the concerned classes instead of few large ones (recommended)",
                                "\n    * provide pre-defined validation polygons via the valData argument",
                                "\n    * decrease trainPartition",
                                "\n    * run without independent validation."))
        }
        valData   <- trainData[-training,]
        trainData <- trainData[training,]
    }
    
    
    if(identical(trainData, valData)) stop("trainData is the same as valData")
    if(!is.null(valData)){
        
        if(trainDataType == "polygons" ){
            ## Clip validation data to training data + 2 pixel buffer 
            trainBuff <- if(minDist > 0) .omniBuffer(trainData, minDist = minDist, img = img) else gUnionCascaded(trainData)
            clip      <- gDifference(valData, trainBuff, byid = TRUE)
            if(is.null(clip)) stop("After clipping valData to trainData+minDist*pix buffer no validation polygons remain. Please provide non-overlapping trainData and valData.")
            
            ## Add class labels back to polygons
            classVec <- data.frame(x = over(clip, valData)[[responseCol]])
            valData  <- as(clip, "SpatialPolygonsDataFrame")
            valData@data <- classVec 
            colnames(valData@data) <- responseCol
            
        } else { 
            ## Check for and deal with overlapping training & validation data                 
            dummy <- if(minDist != 0) .omniBuffer(trainData, minDist = minDist, img = img)  else trainData
            inter <- gIntersects(valData, dummy, byid = T)
            if(any(inter)){            
                inter <- colnames(inter)[which(inter, arr.ind = TRUE)[,2]]
                valData <- valData[!rownames(valData@data) %in% inter,]
            }
        }     
    }
    ## Create hold out indices on polygon level
    if(polygonBasedCV){
        folds        <- createFolds(trainData@data[[responseCol]], k = kfold)
        names(folds) <- NULL
        folds        <- melt(folds) 
        foldCol      <- "excludeFromFold"
        trainData@data[[foldCol]] <- folds[order(folds$value),"L1"]
    } else {
        foldCol <- NULL
    }
    
    ## Calculate area weighted number of samples per polygon
    ## we'll end up with n > nSamples, but make sure to sample each polygon at least once
    .vMessage("Begin sampling training data")
    dataList  <- .samplePixels(SHAPE = trainData, RASTER=img, responseCol = responseCol, nSamples = nSamples)
    dataSet   <- dataList[[1]]
    if(polygonBasedCV) {
        indexOut <- dataSet[[foldCol]]
        dataSet[[foldCol]] <- NULL
    }
    
    ## Unique classes
    if(mode == "classification"){   
        if(!is.factor(dataSet$response)) dataSet$response <- as.factor(dataSet$response)
        classes 	 <- unique(dataSet$response)
        classMapping <- data.frame(classID = as.numeric(classes), class = as.character(classes), stringsAsFactors = FALSE)
        classMapping <- classMapping[order(classMapping$classID),]
        rownames(classMapping) <- NULL
    }
    
    ## Meaningless predictors
    uniqueVals  <- apply(dataSet, 2, function(x){length(unique(x))}) == 1
    if(uniqueVals[1]) stop("Response (responseCol in trainData) contains only one value. Classification doesn't make sense in this case.")
    if(any(uniqueVals)) {
        warning( "Samples from ", paste0(colnames(dataSet)[uniqueVals], collapse = ", "), " contain only one value. The variable will be omitted from model training.")
        dataSet <- dataSet[, !uniqueVals, drop=FALSE]
    }
    
    ## TRAIN ######################### 
    .vMessage("Starting to fit model")   
    .registerDoParallel()
    indexIn <- if(polygonBasedCV) lapply(1:kfold, function(x) which(x != indexOut)) 
    if(model == "mlc") model = mlcCaret
    set.seed(seeds[[1]])
    caretModel 	<- train(response ~ ., data = dataSet, method = model, tuneLength = tuneLength, 
            trControl = trainControl(method = "cv", number = kfold, index = indexIn, savePredictions = "final"), ...)   
    modelFit <- getTrainPerf(caretModel)
    
    dataType <- NULL  
    if(mode == "classification") {
        ## Don't know whether we need this, who would be crazy enough to do more than 255 classes...
        dataType <- if(length(classes) < 255) "INT1U" else "INT2U"
        modelFit <- list(modelFit, confusionMatrix(caretModel, norm = "average"))     
    } 
    
    ## PREDICT ######################### 
    if(predict){
        progress <- "none"
        .vMessage("Starting spatial predict")
        if(verbose)  progress <- "text"
        
        wrArgs          <- list(filename = filename, progress = progress, datatype = dataType, overwrite = overwrite)
        wrArgs$filename <- filename ## remove filename from args if is.null(filename) --> standard writeRaster handling applies
        if(predType == "prob") {
            ddd     <- predict(caretModel, dataSet[1:2,-1,drop=FALSE], type="prob")
            probInd <- 1:ncol(ddd)
        } else {
            probInd <- 1
        } 
        
        spatPred        <- .paraRasterFun(img, rasterFun=raster::predict, args = list(model=caretModel, type = predType, index = probInd), wrArgs = wrArgs)
        if(predType != "prob") names(spatPred) <- responseCol
    } else {
        spatPred <- "No map was produced (predict = FALSE)."
    }
    
    ## VALIDATION ########################
    if(!is.null(valData)){
        nSamplesV <- unlist(Map("max", nSamples, 500))
        
        if(predict & (predType == "raw")){
            set.seed(seeds[[kfold+2]])
            valiSet  <- .samplePixels(valData, spatPred, responseCol = responseCol, nSamples = nSamplesV,  trainCells = dataList[[2]])[[1]]
            colnames(valiSet) <- c("reference", "prediction")
        } else {
            set.seed(seeds[[kfold+2]])
            val <- .samplePixels(valData, img, responseCol = responseCol, nSamples = nSamplesV, trainCells = dataList[[2]])[[1]]
            pred <- predict(caretModel, val[,-1])
            valiSet <- data.frame(reference = val[,1], prediction = pred)
        }
        
        if(mode == "classification"){
            if(!is.factor(valiSet$reference))  valiSet$reference  <- factor(valiSet$reference, levels = levels(classes))
            if(is.numeric(valiSet$prediction)) valiSet$prediction <- factor(levels(classes)[valiSet$prediction], levels = levels(classes))
            validation <- confusionMatrix(data = valiSet$prediction, reference = valiSet$reference)              
        } else {
            valiSet$residuals <- valiSet$reference - valiSet$prediction
            validation <-  data.frame(RMSE = RMSE(valiSet$prediction, valiSet$reference), Rsquared = R2(valiSet$prediction, valiSet$reference))   
        }
        validation <- list(performance = validation, validationSet = valiSet)
    } else {
        validation <- "No independent validation was performed!"
    }
    
    ## Print summary stats
    if(verbose){
        message(paste0(paste0(rep("*",20), collapse = "")," Model summary " , paste0(rep("*",20), collapse = "")))
        print(caretModel)
        print(modelFit)
        message(paste0(paste0(rep("*",20), collapse = "")," Validation summary " , paste0(rep("*",20), collapse = "")))
        print(validation[[1]])
    }
    
    out <- list(model = caretModel, modelFit = modelFit, validation = validation, map = spatPred)
    if(exists("training")) out <- c(out, trainingPartitionIndices = training)
    if(mode == "classification") out$classMapping <- classMapping 
    structure(out, class = c("superClass", "RStoolbox"))
}


.samplePixels <- function(SHAPE, RASTER, responseCol, trainCells = NULL, nSamples, maxnpix = FALSE){
    
    if(inherits(SHAPE, "SpatialPolygons")){					
        #cells <- cellFromPolygon(RASTER, SHAPE),
        r    <- raster(RASTER)	
        buff <- max(res(RASTER))
        polyCells <- .parXapply(X = 1:nrow(SHAPE), XFUN="lapply", FUN= function(i) { 
                    rc <- crop(r, extent(SHAPE[i,]) + buff)
                    rc <- rasterize(SHAPE[i,], rc, fun = "first", silent=TRUE)
                    xy <- rasterToPoints(rc)[,-3,drop=FALSE]			
                    cellFromXY(RASTER, xy)
                }, envir = environment()) 
        
        area  <- lapply(polyCells, length) 
        resp  <- SHAPE[[responseCol]]
        uresp <- unique(resp)
        totalarea <- vapply(uresp, function(xi) sum(unlist(area[resp == xi])), numeric(1))			
        if(maxnpix) nSamples  <- min(totalarea)
        dataSet   <- lapply(seq_along(polyCells), function(xi) {
                    class <- resp[[xi]]
                    ns <- min(ceiling(nSamples * area[[xi]] / totalarea[which(uresp == resp[[xi]])]), area[[xi]] )
                    if(ns > 0) data.frame(response = class, cells = sample(polyCells[[xi]], ns)) else NULL
                })	
        dataSet <- do.call("rbind", dataSet)
        dataSet <- cbind(dataSet, RASTER[dataSet[,"cells"]])        
    } else {
        dataSet <- data.frame(response = SHAPE[[responseCol]], extract(RASTER, SHAPE, cell=TRUE))
    }
    
    ## Discard duplicate cells
    dubs 	<- !duplicated(dataSet[,"cells"]) & complete.cases(dataSet) & !dataSet[,"cells"] %in% trainCells
    dataSet <- dataSet[dubs,]
    list(dataSet[,setdiff(colnames(dataSet), "cells")], cells = dataSet$cells)
}


#' Calculate buffers around trainingData regardless if they are projected or not
#' 
#' for unprojected data_ projects the data to azimuthal equidistant projection, calculates the buffer and reprojects
#' for projected data only buffers
#' @param x Polygons or Points
#' @param minDist buffer distance (in pixels)
#' @param img raster to query resolution and calculate the buffer in m
#' @noRd 
#' @keywords internal
.omniBuffer <- function(x, minDist, img){
    
    if(!is.projected(x)){
        crx  <- projection(x)
        ## Project to azimuthal equidistant centered on training data center
        exc  <- .extentCenter(extent(x))
        aeqd <- paste0("+proj=aeqd +lat_0=", exc[2]," +lon_0=",exc[1], " +x_0=0 +y_0=0 +ellps=WGS84")
        x    <- spTransform(x, CRS(aeqd))
        
        ## Get raster resolution in projected coordinates
        rastRes <- max(res(projectRaster(img[1,,drop=FALSE], crs = aeqd)))
        
        ## Buffer
        x <- gBuffer(x, width = minDist * rastRes)
        
        ## Transform back
        return(spTransform(x, crx))
    } else {
        return(gBuffer(x, width = minDist * max(res(img))))
    }               
}





#' Predict a raster map based on a superClass model fit.
#' 
#' useful to separate model fitting from spatial prediction, which can take some time.
#' 
#' @method predict superClass
#' @param object superClass object
#' @param img Raster object. Layernames must correspond to layernames used to train the superClass model, i.e. layernames in the original raster image.
#' @param predType Character. Type of the final output raster. Either "raw" for class predictions or "prob" for class probabilities. Class probabilities are not available for all classification models (\link[caret]{predict.train}). 
#' @param filename Character or NULL. Filename for output raster file.
#' @param datatype Datatype of output raster file.
#' @param ... Further arguments passed to writeRaster.
#' @export 
#' @examples 
#' ## Load training data
#' data(rlogo)
#' train <- readRDS(system.file("external/trainingPoints.rds", package="RStoolbox"))
#' 
#' ## Fit classifier 
#' SC 	  <- superClass(rlogo, trainData = train, responseCol = "class", 
#'               model = "rf", tuneLength = 1, predict = FALSE)
#' 
#' map <- predict(SC, rlogo)
predict.superClass <- function(object, img, predType = "raw", filename = NULL, datatype = "INT2U", ...){
    stopifnot(inherits(object, c("RStoolbox", "superClass")))
    model <- object$model
    wrArgs          <- c(list(...), list(filename = filename, datatype = datatype))
    wrArgs$filename <- filename ## remove filename from args if is.null(filename) --> standard writeRaster handling applies
    
    if(predType == "prob") {
        py<-img[1:2]
        py[]<-1
        ddd <- predict(model, py, type="prob")
        probInd <- 1:ncol(ddd)
    } else {
        probInd <- 1
    } 
    .paraRasterFun(img, rasterFun=raster::predict, args = list(model=model, type = predType, index = probInd), wrArgs = wrArgs) 
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
