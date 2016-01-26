#' Extract validation results from superClass objects
#' 
#' @param x superClass object or caret::confusionMatrix
#' @param from Character. 'testset' extracts the results from independent validation with testset. 'cv' extracts cross-validation results.  
#' @param metrics Character. Only relevant in classification mode (ignored for regression models). 
#' Select 'overall' for overall accuracy metrics, 'classwise' for classwise metrics, 
#' 'confmat' for the confusion matrix itself and 'caret' to return the whole caret::confusionMatrix object.
#' @export 
#' @return Returns a data.frame with validation results. 
#' If metrics = 'confmat' or 'caret' wil return a table or the full caret::confusionMatrix object, respectively.
#' @examples
#' library(pls)
#' ## Fit classifier (splitting training into 70\% training data, 30\% validation data)
#' train <- readRDS(system.file("external/trainingPoints.rds", package="RStoolbox"))
#' SC   <- superClass(rlogo, trainData = train, responseCol = "class", 
#'                     model="pls", trainPartition = 0.7)
#' ## Independent testset-validation
#' getValidation(SC)
#' getValidation(SC, metrics = "classwise")
#' ## Cross-validation based 
#' getValidation(SC, from = "cv")
getValidation <- function(x, from = "testset", metrics = "overall"){
    
    stopifnot(inherits(x, c("superClass", "mapValidation", "confusionMatrix")) ,
            metrics %in% c("overall", "classwise", "confmat",  "caret"),
            from %in% c("testset", "cv")
    )   
    if(inherits(x, "mapValidation")) x <- x$performance
    if(inherits(x, "superClass") && from == "testset" && inherits(x$validation, "character")){
        stop("No independent validation was performed during model fitting. Use from='cv' to extract cross-validation performance.")
    }
    if(inherits(x,"confusionMatrix") || x$model$modelType == "Classification"){
        
        if(inherits(x,"confusionMatrix")) {
            confMat <- x
        } else if(from == "cv") {
            confMat <- confusionMatrix(x$model$pred$pred, x$model$pred$obs) 
        } else {
            confMat <- x$validation$performance
        }
        
        if(metrics == "overall") {
            perf <- as.data.frame(t(confMat$overall))
        } else if (metrics == "classwise"){
            perf <- confMat$byClass
            perf <- data.frame(class = gsub("Class:", "", rownames(perf)), perf)
            rownames(perf) <- NULL
        } else if (metrics == "confmat"){
            return(confMat$table)
        } else if (metrics == "caret"){
            return(confMat)
        }
    } else {
        ## Regression
        if(from=="testset"){
            perf <- x$validation$performance
        } else {
            bestPerf <- x$model$bestTune
            colnames(bestPerf) <- gsub("^\\.", "", colnames(bestPerf))
            perf <- merge(x$model$results, bestPerf)[,-c(1:length(bestPerf))]      
        }
    }
    if(inherits(x, "confusionMatrix")) {
        model <- from <- NA
    } else {
        model <- x$model$method
    }
    perf <- data.frame(model = model, validation = from, perf)
    
    return(perf)
}

