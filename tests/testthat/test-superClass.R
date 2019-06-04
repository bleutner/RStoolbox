context("superClass")

suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(pls))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(caret))

data(lsat)
lsat <- lsat[[1:4]]

## Set-up test data
set.seed(1)
poly     <- readRDS(system.file("external/trainingPolygons.rds", package="RStoolbox"))
poly$res <- as.numeric(poly$class)
pts      <- spsample(poly, n = 100, type ="random")
pts      <- SpatialPointsDataFrame(pts, data = over(pts, poly))
ll       <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
polyll   <- spTransform(poly, ll)  
ptsll    <- spTransform(pts, ll)  
lsatll   <- projectRaster(lsat, crs = ll)
lsNA      <- lsat
lsNA[1:100,] <- NA

trainList <- list(projected = list(polygons = poly, points = pts, img = lsat), 
        geographical = list(polygons = polyll, points = ptsll, img = lsatll))



## Maximum likelihood custom model
test_that("maximum likelihood model",
        expect_is(superClass(lsat, trainData = poly, responseCol = "class", model = "mlc", tuneLength = 1, trainPartition = 0.7, predict = FALSE), "superClass")
) 



for(proj in c("projected", "geographical")){
    for(type in c("polygons", "points")){
        
        
        if (!identical(Sys.getenv("NOT_CRAN"), "true") ) {
            if( type == "points") break
        }
        
        info <- paste(c("train type = ", type, " | coordinates = ", proj), collapse = "")       
        train <- trainList[[proj]][[type]]
        img   <- trainList[[proj]][["img"]]
        geometry <- if(type=="polygons") "SpatialPolygons" else "SpatialPoints"
        ## No prediction    
        set.seed(1)
        sc <- superClass(img, trainData = train, nSamples = 50, responseCol = "class", model = "pls", 
                tuneGrid = data.frame(ncomp = 3), tuneLength = 1, trainPartition = 0.7, predict = FALSE)
        
        ## with prediction
        set.seed(1)    
        sc2 <- superClass(img, trainData = train, nSamples = 50, responseCol = "class", model = "pls", 
                tuneGrid = data.frame(ncomp = 3), tuneLength = 1, trainPartition = 0.7, predict = TRUE)
        
        ## Polygonbased CV
        set.seed(1)
        test_that("polygonbased CV", {
                    expect_is(superClass(img, trainData = train, nSamples = 50, responseCol = "class", model = "pls", 
                                    tuneLength = 1, polygonBasedCV = TRUE, trainPartition = 0.7, predict = FALSE), c("RStoolbox", "superClass"))
                })
        
        ## Based on numeric classes
        set.seed(1)
        sc3 <- superClass(img, trainData = train,  nSamples = 50,responseCol = "res", model = "pls", 
                tuneGrid = data.frame(ncomp = 3), tuneLength = 1, trainPartition = 0.7, predict = TRUE)
        
        test_that("validation raster vs pixel based, numeric vs. factor predictors",{
                    expect_is(sc$map, "character", info = info)
                    expect_equal(sc$validation, sc2$validation, info = info)                                
                }) 
        
        test_that("numeric vs. factor predictors", {                        
                    expect_equal(cellStats(sc2$map - sc3$map, sum), 0, info = info)
                })
        
        test_that("validation sample coords and geometries are returned", {                        
                    expect_is(sc$validation$validationGeometry, geometry, info = info) 
                    expect_is(sc2$validation$validationGeometry, geometry, info = info) 
                    expect_is(sc3$validation$validationGeometry, geometry, info = info) 
                    expect_is(sc$validation$validationSamples, "data.frame", info = info) 
                    expect_is(sc2$validation$validationSamples, "data.frame", info = info) 
                    expect_is(sc3$validation$validationSamples, "data.frame", info = info) 
                    expect_equal(colnames(sc$validation$validationSamples), c("reference", "prediction", "cell", "x", "y"), info = info) 
                    expect_equal(colnames(sc2$validation$validationSamples), c("reference", "prediction", "cell", "x", "y"), info = info) 
                    expect_equal(colnames(sc3$validation$validationSamples), c("reference", "prediction", "cell", "x", "y"), info = info) 
                    
                })
        
        
        test_that("predict.superClass map is identical to superClass$map",{
                    expect_true(compareRaster(sc2$map, predict(sc2, img), values = TRUE), info = info)                      
                })
        
        test_that("prediction of probability layers", {   
                    expect_is(sc4  <- superClass(img, trainData = train, nSamples = 50, ntree = 100, responseCol = "class", model = "rf",
                                    predType = "prob", tuneLength = 1, predict = TRUE),  "superClass", info = info)
                    expect_is(scp <- predict(sc4, img, predType = "prob"), "RasterBrick")
                    expect_identical(nlayers(sc4$map), 4L, info=info)
                    expect_true(compareRaster(sc4$map, scp, values = TRUE), info = info) 
                    expect_equal(unique(round(sum(sc4$map))) , 1, info = info)            
                })
        
        test_that("external valData instead of trainPartition",{
                    set.seed(1)
                    valInd   <- createDataPartition(train[["class"]])[[1]]
                    expect_is(sc <- superClass(img, trainData = train[valInd,], valData = train[-valInd,], nSamples = 50, responseCol = "class", model = "pls", 
                                    mode = "classification", predict = FALSE), "superClass", info = info)    
                    expect_is(sc$validation$performance, "confusionMatrix")
                }) 
        
        
        test_that("regression mode works", {
                    expect_is(sc2 <- superClass(img, trainData = train, trainPartition=.7, tuneLengt = 1, nSamples = 50, responseCol = "res", model = "pls", 
                                    mode = "regression", predict = FALSE), "superClass", info = info)
                })
        
        test_that("superClass works with a single RasterLayer", {
                    skip_on_cran()
                    expect_is(sc2 <- superClass(img[[1]], trainData = train, trainPartition=.7, nSamples = 50, ntree = 50, tuneLength = 1, responseCol = "res", model = "rf", 
                                    mode = "classification", predict = FALSE), "superClass", info = info)
                    expect_is(sc2 <- superClass(img[[1]], trainData = train, trainPartition=.7, nSamples = 50,ntree = 50, tuneLength = 1, responseCol = "res", model = "rf", 
                                    mode = "classification", predict = TRUE), "superClass", info = info)
                    skip_on_cran()
                    expect_is(sc2 <- superClass(img[[1]], trainData = train, trainPartition=.7, nSamples = 50, tuneGrid = data.frame(ncomp = 1), responseCol = "res", model = "pls", 
                                    mode = "regression", predict = TRUE), "superClass", info = info)
                    expect_is(sc2 <- superClass(img[[1]], trainData = train, trainPartition=.7, nSamples = 50, tuneGrid = data.frame(ncomp = 1), responseCol = "res", model = "pls", 
                                    mode = "regression", predict = FALSE), "superClass", info = info)
                })
        if(proj == "projected"){
            projection(img)   <- NA
            projection(train) <- NA
            test_that("missing projection info", {
                        expect_warning(superClass(img, trainData = train, minDist = 1, nSamples = 50, responseCol = "class", model = "pls", 
                                        tuneGrid = data.frame(ncomp = 3), tuneLength = 1, trainPartition = 0.7, predict = FALSE), "missing projection")
                    })
        }
    }
    
}


if (identical(Sys.getenv("NOT_CRAN"), "true") ) {
    ## Tiny raster bug caused superClass to fail when predictions were written to .grd file 
    test_that("NA in raster remains NA",{
                expect_is(sc <- superClass(lsNA, trainData = pts, responseCol = "class", model = "rf", filename = rasterTmpFile(), trainPartition = 0.7, predict = TRUE), "superClass")
                expect_equal(sum(is.na(sc$map[1:100,])), 100*ncol(lsNA)) 
                expect_false(anyNA(sc$map[101:nrow(lsNA),]))            
            }) 
    
    
    ## Checks after clipping 
    test_that("fails if no validation points remain after clipping",{
                expect_error(sc <- superClass(lsNA, trainData = pts, minDist=1000, responseCol = "class", trainPartition = 0.7), "no validation points remained")
                expect_error(sc <- superClass(lsNA, trainData = poly, minDist=1000, responseCol = "class", trainPartition = 0.7), "no validation polygons remain")
            })
    
    ## Projection checks
    test_that("projection mismatch errors", 
            expect_error(superClass(trainList[[1]]$img, trainData = trainList[[2]]$polygons, responseCol = "class"), "must have the same projection")
    )
    
}

