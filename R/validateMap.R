
#' Map accuracy assessment
#'
#' validate a map from a classification or regression model. This can be useful to update the accuracy assessment after filtering, e.g. for a minimum mapping unit.
#'  
#' @param map RasterLayer. The classified map.
#' @param valData SpatialPolygonsDataFrame or SpatialPointsDataFrame with validation data.
#' @param nSamples Integer. Number of pixels to sample for validation (only applies to polygons).
#' @param responseCol Character. Column containing the validation data in attribute table of \code{valData}.
#' @param mode Character. Either 'classification' or 'regression'.
#' @param classMapping optional data.frame with columns \code{'class'} and \code{'classID'} defining the mapping from raster integers to class names. 
#' @note 
#' Polygons, which are smaller than the map resolution will only be considered if they overlap with a pixel center coordinate, otherwise they will be ignored.
#' 
#' @export 
#' @examples 
#' \dontrun{
#' library(caret)
#' library(raster)
#' 
#' ## Training data
#' data(lsat)
#' poly     <- readRDS(system.file("external/trainingPolygons.rds", package="RStoolbox"))
#' 
#' ## Split training data in training and validation set (50%-50%)
#' splitIn   <- createDataPartition(poly$class, p = .5)[[1]]
#' train <- poly[splitIn,]
#' val   <- poly[-splitIn,]
#' 
#' ## Classify (deliberately poorly)
#' sc <- superClass(lsat, trainData = train, responseCol = "class", nSamples = 50, model = "mlc")
#' 
#' ## Polish map with majority filter
#' 
#' polishMap <- focal(sc$map, matrix(1,3,3), fun = modal) 
#' 
#' ## Validation
#' ## Before filtering
#' val0 <- validateMap(sc$map, valData = val, responseCol = "class", classMapping = sc$classMapping)
#' ## After filtering
#' val1 <- validateMap(polishMap, valData = val, responseCol = "class", classMapping = sc$classMapping)
#' }
validateMap <- function(map, valData, responseCol, nSamples = 500,  mode = "classification", classMapping = NULL){
    
    stopifnot(responseCol %in% names(valData), mode %in% c("classification", "regression"))
    
    valiSet  <- .samplePixels(SHAPE = valData, RASTER = map, responseCol = responseCol, nSamples = nSamples,  trainCells = NULL)
    colnames(valiSet[[1]]) <- c("reference", "prediction") 
    if(mode=="classification") {  
        if(!is.null(classMapping)) {
            valiSet[[1]][,"prediction"] <- classMapping[match(valiSet[[1]][,"prediction"], classMapping$classID),"class"]
        } 
        performance = confusionMatrix(valiSet[[1]][,"prediction"], reference = valiSet[[1]][,"reference"])
    } else {
        performance = postResample(pred = valiSet[[1]][,"prediction"], obs = valiSet[[1]][,"reference"])    
    }
    valiSet <- do.call("cbind",valiSet)
    colnames(valiSet) <- c("reference", "prediction", "cell")
    out <- list(performance = performance, validationSet = valiSet)
    structure(out, class = c("mapValidation", "RStoolbox"))
    
}



#' @method print mapValidation
#' @export 
print.mapValidation <- function(x,...){
    cat("$performance\n")
    print(x$performance)
    cat("\n$validationSet\n")
    if(nrow(x$validationSet) < 6){
        print(x$validationSet)
    } else {       
        cat("Total number of validation samples: ", nrow(x$validationSet), "\n")
        cat("Validation samples per class:")
        print(table(x$validationSet$reference))
        cat("\n")
        print(x$validationSet[c(1:3),])
        cat("...\n")
        write.table(format(tail(x$validationSet,3), justify="right"), col.names=F, quote=F)      
    }
}