#' Supervised Classification
#' 
#' Supervised classification both for classification and regression mode based on vector training data (points or polygons). 
#' 
#' @param img SpatRaster. Typically remote sensing imagery, which is to be classified.
#' @param trainData sf or sp spatial vector data containing the training locations (POINTs,or POLYGONs).
#' @param valData  Ssf or sp spatial vector data containing the validation locations (POINTs,or POLYGONs) (optional).
#' @param responseCol Character or integer giving the column in \code{trainData}, which contains the response variable. Can be omitted, when \code{trainData} has only one column.
#' @param nSamples Integer. Number of samples per land cover class. If \code{NULL} all pixels covered by training polygons are used (memory intensive!). Ignored if trainData consists of POINTs.
#' @param nSamplesV Integer. Number of validation samples per land cover class. If \code{NULL} all pixels covered by validation polygons are used (memory intensive!). Ignored if valData consists of POINTs.
#' @param polygonBasedCV Logical. If \code{TRUE} model tuning during cross-validation is conducted on a per-polygon basis. Use this to deal with overfitting issues. Does not affect training data supplied as SpatialPointsDataFrames.
#' @param trainPartition Numeric. Partition (polygon based) of \code{trainData} that goes into the training data set between zero and one. Ignored if \code{valData} is provided.
#' @param model Character. Which model to use. See \link[caret]{train} for options. Defaults to randomForest ('rf'). In addition to the standard caret models, a maximum likelihood classification is available via \code{model = 'mlc'}. 
#' @param tuneLength Integer. Number of levels for each tuning parameter (see \link[caret]{train} for details).
#' @param kfold Integer. Number of cross-validation resamples during model tuning.
#' @param sampling Character. Describes the type of additional sampling that is conducted after resampling (usually to resolve class imbalances), from caret. Currently supported are \code{up}, \code{down}, \code{smote}, and \code{rose}. Note, that \code{smote} requires the packages \code{themis} and \code{rose} the package \code{ROSE}. Latter is noly for binary classification problems.
#' @param minDist Numeric. Minumum distance between training and validation data,
#'  e.g. \code{minDist=1} clips validation polygons to ensure a minimal distance of one pixel (pixel size according to \code{img}) to the next training polygon. 
#' Requires all data to carry valid projection information.
#' @param mode Character. Model type: 'regression' or 'classification'. 
#' @param predict Logical. Produce a map (TRUE, default) or only fit and validate the model (FALSE).
#' @param predType Character. Type of the final output raster. Either "raw" for class predictions or "prob" for class probabilities. Class probabilities are not available for all classification models (\link[caret]{predict.train}). 
#' @param filename Path to output file (optional). If \code{NULL}, standard raster handling will apply, i.e. storage either in memory or in the raster temp directory. 
#' @param verbose Logical. prints progress and statistics during execution
#' @param overwrite logical. Overwrite spatial prediction raster if it already exists.
#' @param ... further arguments to be passed to \code{\link[caret]{train}}
#' @details
#' Note that superClass automatically loads the lattice and randomForest package.
#' SuperClass performs the following steps:
#' 
#' \enumerate{
#' \item Ensure non-overlap between training and validation data. This is neccesary to avoid biased performance estimates.
#'  A minimum distance (\code{minDist}) in pixels can be provided to enforce a given distance between training and validation data.
#' 
#' \item Sample training coordinates. If \code{trainData} (and \code{valData} if present) are polygons \code{superClass} will calculate the area per polygon and sample
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
#' @return A superClass object (effectively a list) containing:
#' \enumerate{
#' \item $model: the fitted model
#' \item $modelFit: model fit statistics
#' \item $training: indexes of samples used for training
#' \item $validation: list of
#'     \enumerate{
#'        \item $performance: performance estimates based on independent validation (confusion matrix etc.)
#'        \item $validationSamples: actual pixel coordinates plus reference and predicted values used for validation
#'        \item $validationGeometry: validation polygpns (clipped with mindist to training geometries)
#'     }
#' \item $map: the predicted raster
#' \item $classMapping: a data.frame containing an integer <-> label mapping
#' }  
#' @seealso \code{\link[caret]{train}} 
#' @export
#' @examples 
#' library(RStoolbox)
#' library(caret)
#' library(randomForest)
#' library(e1071)
#' library(terra)
#' train <- readRDS(system.file("external/trainingPoints_rlogo.rds", package="RStoolbox"))
#' 
#' ## Plot training data
#' olpar <- par(no.readonly = TRUE) # back-up par
#' par(mfrow=c(1,2))
#' colors <- c("yellow", "green", "deeppink")
#' plotRGB(rlogo)
#' plot(train, add = TRUE, col =  colors[train$class], pch = 19)
#' 
#' ## Fit classifier (splitting training into 70\% training data, 30\% validation data)
#' SC       <- superClass(rlogo, trainData = train, responseCol = "class",
#' model = "rf", tuneLength = 1, trainPartition = 0.7)
#' SC
#' 
#' ## Plots
#' plot(SC$map, col = colors, legend = FALSE, axes = FALSE, box = FALSE)
#' legend(1,1, legend = levels(train$class), fill = colors , title = "Classes", 
#' horiz = TRUE,  bty = "n")
#' par(olpar) # reset par
superClass <- function(img, trainData, valData = NULL, responseCol = NULL,
        nSamples = 1000, nSamplesV = 1000, polygonBasedCV = FALSE, trainPartition = NULL,
        model = "rf", tuneLength = 3,  kfold = 5, sampling = NULL,
        minDist = 2,  mode = "classification", predict = TRUE, predType = "raw",
        filename = NULL, verbose,
        overwrite = TRUE, ...) {
    # TODO: check for empty factor levels
    # TODO: consider splitting large polygons if there are few polygons in total
    
    if(!missing("verbose")) .initVerbose(verbose)
    verbose <- getOption("RStoolbox.verbose") 
    
    if(!inherits(img, 'Raster') && !inherits(img,"SpatRaster"))
      stop("img must be a raster object (RasterLayer,RasterBrick or RasterStack)", call.=FALSE)

    img <- .toTerra(img)

    trainData <- .toSf(trainData)
    if(!missing("valData")) valData <- .toSf(valData)
    
    ## Object types
    trainDataType <- st_geometry_type(trainData, by_geometry = FALSE)
    if(!trainDataType %in% c("POLYGON", "POINT")) 
        stop("traingData must POINTs or a POLYGONs", call.=FALSE)
    
    ## Attribute column
    if(is.numeric(responseCol)) responseCol <- colnames(trainData)[responseCol]
    tdataCols <- setdiff(colnames(trainData), attr(trainData, "sf_column"))
    if(!is.null(valData)) vDataCols <- setdiff(colnames(valData), attr(valData, "sf_column"))
    
    ## Sanitize arguments (polygonBasedCV is only relevant for polygons)
    if(trainDataType == "POLYGON" & polygonBasedCV) polygonBasedCV <- FALSE
    
    if(is.null(responseCol)){
        if(length(tdataCols) == 1) {
            responseCol <- tdataCols
            .vMessage(sprintf("You did not specify the responseCol argument. \nSince your trainData only contains one data column ('%s') it is assumed this is it", responseCol))
        } else {
            stop(paste("Dont't know which column in trainData contains the class attribute. \nPlease specify responseCol as one of: ", paste(tdataCols,collapse=", ")), call. = FALSE)
        }
    }    
    if(!responseCol %in% tdataCols)
        stop(paste0("The column ", responseCol, " does not exist in trainData. \nAvailable columns are: ", paste0(tdataCols,collapse=", ")), call. = FALSE) 
    if(!is.null(valData) && !responseCol %in% vDataCols) 
        stop(paste0("The column ", responseCol, " does not exist in valData. \nAvailable columns are: ", paste0(vDataCols,collapse=", ")), call. = FALSE) 
    #if(!is.null(valData) && !all.equal(class(trainData), class(valData)))
    #   stop("trainData and valData must be of the same class. Either SpatialPointsDataFrame or SpatialPolygonsDataFrame.")
    if(any(!mode %in% c("regression", "classification"))) 
        stop("unknown mode. must be 'regression' or 'classification'")
    
    ## Check projections
    if(st_crs(img) != st_crs(trainData) | (!is.null(valData) && st_crs(img) != st_crs(valData)) )
        stop("img, trainData and valData (if provided) must have the same projection")

    ## Check overlap of vector and raster data
    trainData <- .selectIntersecting(img, trainData)
    if(!is.null(valData)) {
        valData <- .selectIntersecting(img, valData)
    }
    valData <- valData
    trainPartition <- trainPartition
    
    ## Make sure classification is run with factors
    if(mode == "classification") {
        if(!is.null(valData) && (class(trainData[[responseCol]]) != class(valData[[responseCol]]))) {
            stop("response columns in trainData and valData must be of the same type, i.e. both integer, character or factor")
        }

        if(!is.factor(trainData[[responseCol]])) {
            trainData[[responseCol]] <- as.factor(trainData[[responseCol]])
            if(!is.null(valData)) {
                valData[[responseCol]] <- factor(valData[[responseCol]], levels = levels(trainData[[responseCol]]))
            }
        }
        
        ## Unique classes
        classes      <- sort(unique(trainData[[responseCol]]))
        classMapping <- data.frame(classID = as.numeric(classes), class = classes, stringsAsFactors = FALSE)
        classMapping <- classMapping[order(classMapping$classID),]
        rownames(classMapping) <- NULL
    } else {
        classMapping <- NULL
    }
    
    ## Spit ellipsis into caret::trainControl and terra::writeRaster
#    frmlsrain <- names(formals(terra::writeRaster))
#    args  <- c(list(...), method = method)
#    argsrainControl  <- args[names(args) %in% frmlsrain]
#    args_writeRaster   <- args[!names(args) %in% frmlsrain]
#    args_writeRaster$filename <- if(length(classes) == 1) filename else NULL ## write raster here already during predict if only one layer is output
    
    ## Split into training and validation data (polygon basis)
    if(is.null(valData) & !is.null(trainPartition)){
        training  <- createDataPartition(trainData[[responseCol]], p = trainPartition)[[1]] ## this works for polygons as well because every polygon has only one entry in the attribnute table
        hint <- paste0("\n   You could either ",
                "\n    * provide more (often smaller) polygons for the concerned classes instead of few large ones (recommended)",
                "\n    * provide pre-defined validation polygons via the valData argument",
                "\n    * decrease trainPartition",
                "\n    * run without independent validation.")
        if(length(training) == nrow(trainData))
            stop(paste0("There are not enough polygons/points to split into training and validation partitions. \n  You could either ",
                            hint))
        if(mode == "classification"){
            valVal  <- trainData[[responseCol]][-training]
            valDiff <- setdiff(trainData[[responseCol]], valVal)
            if(length(valDiff)) stop(paste0("The independent validation partition does not encompass all classes.",
                                "\n   Missing classes: ", paste(valDiff, collapse = ", "), hint
                        ))
        }
        valData   <- trainData[-training,]
        trainData <- trainData[ training,]
    } else {
        training <- seq_len(NROW(trainData))
    }

    if(identical(trainData, valData))
        stop("trainData is the same as valData")

    if(!is.null(valData)){
        if(is.na(st_crs(trainData)) & minDist > 0) {
            warning("trainData is missing projection information and thus cannot be buffered. minDist will be set to zero.", call. = FALSE)
            minDist <- 0
        }

        ## Clip validation data to training data + 2 pixel buffer
        trainBuff <- if(minDist > 0) st_buffer(trainData, minDist*max(res(img)))  else trainData
        st_agr(valData) <- "constant"
        valData <- st_difference(valData, st_geometry(st_union(trainBuff)))

        if(!nrow(valData)){
            stop(paste0("After applying a buffer of ",minDist," pixels (minDist) no validation points remained.",
                            "\nPossible solutions:",
                            "\n * split datasets yourself, i.e. provide valData instead of trainPartition",
                            "\n * reduce minDist (may cause optimistic bias in validation!)",
                            "\n * provide more trainingPoints which are well spread across the scene"
                    ), call. = FALSE)
        }

        if(mode == "classification" && !all(classMapping$class %in% valData[[responseCol]])){
            stop(paste0("After applying a buffer of ",minDist," pixels (minDist) validation not all classes are represented in the validation set.",
                            "\nPossible solutions:",
                            "\n * split datasets yourself, i.e. provide valData instead of trainPartition",
                            "\n * reduce minDist (may cause optimistic bias in validation!)",
                            "\n * provide more trainingPoints which are well spread across the scene"
                    ), call. = FALSE)
        }


    }

    ## Create hold out indices on polygon level

    if(trainDataType == "POLYGON" && polygonBasedCV){
        foldCol      <- "excludeFromFold"
        trainData <- do.call(rbind, lapply(classes, function(ci) {
            sub          <- trainData[trainData[[responseCol]] == ci,]
            folds        <- createFolds(sub[[responseCol]], k = kfold)
            names(folds) <- NULL
            folds        <- melt(folds)
            sub[[foldCol]] <- folds[order(folds$value),"L1"]
            sub
        }))
    } else {
        foldCol <- NULL
        polygonBasedCV <- FALSE
    }

    ## Calculate area weighted number of samples per polygon
    ## we'll end up with n > nSamples, but make sure to sample each polygon at least once
    .vMessage("Begin sampling training data")
    dataList  <- .samplePixels(trainData, img, responseCol = responseCol, nSamples = nSamples, foldCol = foldCol)

    dataSet <- dataList[[1]]
    if(trainDataType == "POLYGON" && polygonBasedCV) {
        indexOut <- dataList[[2]][[foldCol]]
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
    indexIn <- if(polygonBasedCV) lapply(1:kfold, function(x) which(x != indexOut))

    if(model == "mlc")
      model <- mlcCaret

    caretModel     <- train(response ~ ., data = dataSet, method = model, tuneLength = tuneLength,
            trControl = trainControl(method = "cv", classProbs = {predType=="prob"},
                    number = kfold, index = indexIn, savePredictions = "final", sampling = sampling), ...)
    modelFit <- getTrainPerf(caretModel)
    
    dataType <- NULL
    if(mode == "classification") {
        dataType <- "INT2S"
        modelFit <- list(modelFit, confusionMatrix(caretModel, norm = "average"))
    }
    
    ## PREDICT ######################### 
    if(predict){
        .vMessage("Starting spatial predict")

        if(predType == "prob") {
            ddd     <- predict(caretModel, dataSet[1:2,-1, drop = FALSE], type="prob")
            probInd <- 1:ncol(ddd)
        } else {
            probInd <- 1
        }

        ## Use this, once terra is mature enough
        # nc <- .getNCores()
        # progress <- if(verbose)  2 else 1
        # if(is.null(filename)) filename<-""
        # wrArgs   <- list(progress = progress, datatype = dataType)
        # spatPred <- terra::predict(img, model=caretModel, type = predType, index = probInd, cpkgs = c("caret"),
        # filename = filename , overwrite = overwrite, cores = nc,  wopt = wrArgs)

        progress <- if(verbose) "text" else "none"
        wrArgs <- list(filename = filename, progress = progress, datatype = dataType, overwrite = overwrite)
        wrArgs$filename <- filename ## remove filename from args if is.null(filename) --> standard writeRaster handling applies
        spatPred <- .paraRasterFun(img, rasterFun=terra::predict, args = list(model=caretModel, type = predType, index = probInd, na.rm = T), wrArgs = wrArgs)
        if(predType != "prob")
          names(spatPred) <- paste0(responseCol, "_supervised")

    } else {
        spatPred <- "No map was produced (predict = FALSE)."
    }
    
    ## VALIDATION ########################
    if(!is.null(valData)){
        .vMessage("Begin validation")
        if(predict & (predType == "raw")){
            ## We have a predicted raster already, so we can extract predictions directly
            valiSet  <- .samplePixels(valData, spatPred, responseCol = responseCol, nSamples = nSamplesV,
                    trainCells = dataList[[2]][,"cell"], withXY = TRUE, classMapping = classMapping)
            colnames(valiSet[[1]]) <- c("reference", "prediction")
            valiSet     <- data.frame(valiSet[[1]], valiSet[[2]])
        } else {
            ## We don't have a predicted raster, hence we need to run predict on data samples first
            val     <- .samplePixels(valData, img, responseCol = responseCol, nSamples = nSamplesV,
                    trainCells = dataList[[2]][,"cell"], withXY = TRUE, classMapping = classMapping)
            pred    <- predict(caretModel, val[[1]][,-1,drop=FALSE])
            valiSet <- data.frame(reference = val[[1]][,1], prediction = pred, val[[2]])
        }

        if(mode == "classification"){
            if(!is.factor(valiSet$reference))  valiSet$reference  <- factor(valiSet$reference, levels = levels(classes))
            if(is.numeric(valiSet$prediction)) {
                if(predict){ ## temporary workaround for raster grd/INT1U/NA bug
                    pr <- valiSet$prediction
                    if(!"0" %in% levels(classes)) pr[pr==0] <- NA
                }
                valiSet$prediction <- factor(levels(classes)[pr], levels = levels(classes))
            }
            validation <- confusionMatrix(data = as.factor(valiSet$prediction), reference = as.factor(valiSet$reference))
        } else {
            valiSet$residuals <- valiSet$reference - valiSet$prediction
            validation <-  data.frame(RMSE = .rmse(valiSet$prediction, valiSet$reference),
                    Rsquared = cor(valiSet$prediction, valiSet$reference, use = "complete.obs")^2)
        }
        valiSet <- st_as_sf(valiSet, coords = c("x", "y"), crs = st_crs(valData))
        validation <- list(performance = validation,
                validationSamples = valiSet, validationGeometry = valData )
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
    
    out <- list(model = caretModel, modelFit = modelFit, training = list(trainingPartition=training), validation = validation, map = spatPred)

    if(mode == "classification") out$classMapping <- classMapping
    structure(out, class = c("superClass", "RStoolbox"))
}

#' Check overlap between vector and raster data
#' Note CRS equality is assumed and not tested anymore (tested earlier)
#' this was introduced to avoid spurious CRS mismatches  
#' 
#' @param img SpatRaster or any other geometry which returns a st_bbox
#' @param vect sf 
#' @keywords internal
#' @noRd
.selectIntersecting <- function(img,vect) {
    ext <- st_as_sfc(st_bbox(ext(img)))
    st_crs(ext) <- st_crs(vect)

    overlap <- as.vector(st_intersects(ext, vect, sparse = FALSE))
    so <- sum(!overlap)
    if (so)
        warning(sprintf("Some trainData (%s/%s) do not overlap with img", so, length(overlap)), call. = FALSE)
    if(!sum(overlap))
        stop("img and trainData do not overlap", call. = FALSE)
    return(vect[overlap,])
}

.samplePixels <- function(v, r, responseCol, trainCells = NULL, nSamples = NULL, maxnpix = FALSE, withXY = FALSE, foldCol = NULL, classMapping = NULL){
    isPoint <- st_geometry_type(v, by_geometry = FALSE) == "POINT"
    if(isPoint)
        nSamples <- NULL
    ## exactextractr cannot handle point data. Therefore we convert to polygons, and pick the pixel with the highest coverage.
    ## This is a workaround, but still faster than other extract methods.
    v <- st_buffer(v,xres(r)*0.1,nQuadSegs=1)

    nSampsCol <- NULL
    if (!is.null(nSamples)) {
        v$area <- st_area(v)
        v <- v %>% group_by( group = get(responseCol)) %>%
                  mutate( nSamps = max(as.integer(ceiling(area/sum(area) * nSamples)),1)) %>%
                    dplyr::select(-"group")
        nSampsCol <- "nSamps"
    }

    cols <- c(responseCol, if(nlyr(r)>1) names(r) else "value", "cell", "x"[withXY], "y"[withXY])
    dataSet <- exact_extract(r,v, fun = function(vals, ...) {

                ## Filter
                if(isPoint) vals <- vals[which.max(vals$coverage_fraction),] ## POINT extraction workaround
                cc  <- complete.cases(vals)
                cc2 <- if(!is.null(trainCells)) !vals[,"cell"] %in% trainCells else TRUE
                cc3 <- if(!isPoint) vals$coverage_fraction == 1 else TRUE
                vals <- vals[cc & cc2 & cc3, ]

                if (is.null(nSamples)|| nrow(vals) <= 1) {
                    return(vals[,cols])
                }

                ## Random Sample
                vals[sample(1:nrow(vals), min(v$nSamps[1], nrow(vals))), cols]
            },
            summarize_df = TRUE, force_df = TRUE, include_cell = TRUE,
            include_cols = c(responseCol, nSampsCol), include_xy = withXY, progress = FALSE)

    ## Discard duplicate cells (possible from two polygons over the same pixel)
    dataSet <- dataSet[!duplicated(dataSet[,"cell"]),]
    s <- colnames(dataSet) %in% c( "cell","x","y", foldCol)
    colnames(dataSet)[!s] <- c("response", names(r))

    ## Convert int to factors
    if(!is.null(classMapping) && is.numeric(dataSet[[responseCol]]) ) {
        m <- match(dataSet[[responseCol]], classMapping$classID)
        dataSet[[responseCol]] <- classMapping[m,"class"]
    }
    list(dataSet[,!s], cells = dataSet[, s, drop=FALSE])
}

########################################
## DEPRACATED, since we can now conveniently buffer latlong in sf or terra. How cool is that :-)
########################################
##' Calculate buffers around trainingData regardless if they are projected or not
##' 
##' for unprojected data_ projects the data to azimuthal equidistant projection, calculates the buffer and reprojects
##' for projected data only buffers
##' @param x Polygons or Points
##' @param minDist buffer distance (in pixels)
##' @param img raster to query resolution and calculate the buffer in m
##' @noRd 
##' @keywords internal
#.omniBuffer <- function(x, minDist, img){
#    
#    if(!is.projected(x)){
#        crx  <- projection(x)
#        ## Project to azimuthal equidistant centered on training data center
#        exc  <- .extentCenter(extent(x))
#        aeqd <- paste0("+proj=aeqd +lat_0=", exc[2], " +lon_0=", exc[1], " +x_0=0 +y_0=0 +ellps=WGS84")
#        x    <- spTransform(x, CRS(aeqd))
#        
#        ## Get raster resolution in projected coordinates
#        rastRes <- max(res(projectRaster(img[1,,drop=FALSE], crs = aeqd)))
#        
#        ## Buffer
#        x <- gBuffer(x, width = minDist * rastRes)
#        
#        ## Transform back
#        return(spTransform(x, crx))
#    } else {
#        return(gBuffer(x, width = minDist * max(res(img))))
#    }               
#}


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
#' train <- readRDS(system.file("external/trainingPoints_rlogo.rds", package="RStoolbox"))
#' 
#' ## Fit classifier 
#' SC       <- superClass(rlogo, trainData = train, responseCol = "class",
#'               model = "rf", tuneLength = 1, predict = FALSE)
#' 
#' map <- predict(SC, rlogo)
predict.superClass <- function(object, img, predType = "raw", filename = NULL, datatype = "INT2U", ...){
    stopifnot(inherits(object, c("RStoolbox", "superClass")))
    
    ## extract model (avoid copying entire object to SOCK clusters in .paraRasterFun - I think / not validated)
    model <- object$model
    img <- .toTerra(img)
    if(predType == "prob") {
        py<-img[1:2]
        py[]<-1
        ddd <- predict(model, py, type="prob")
        probInd <- 1:ncol(ddd)
    } else {
        probInd <- 1
    } 
    
    ## Still waiting for terra to mature 
    #    wrArgs   <- c(list(...), list( datatype = datatype))
    #    spatPred <- terra::predict(img, model=caretModel, type = predType, index = probInd, cpkgs = c("caret"),
    #            filename = filename , overwrite = overwrite, cores = nc,  wopt = wrArgs)
    
    wrArgs          <- c(list(...), list(filename = filename, datatype = datatype))
    wrArgs$filename <- filename ## remove filename from args if is.null(filename) --> standard writeRaster handling applies
    .paraRasterFun(img, rasterFun=terra::predict, args = list(model=model, type = predType, index = probInd, na.rm = TRUE), wrArgs = wrArgs)

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
