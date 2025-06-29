context("superClass")
suppressPackageStartupMessages(library(terra))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(pls))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(caret))

lsat_t <- lsat
lsat_t <- lsat_t[[1:4]]
## Set-up test data
set.seed(1)
poly     <- readRDS(system.file("external/trainingPolygons_lsat.rds", package="RStoolbox"))
poly$res <- as.numeric(poly$class)
poly <- st_as_sf(poly)
pts <- st_join(st_as_sf(st_sample(poly, 100, type = "regular")), poly)
lsNA     <- lsat_t
lsNA[1:100,] <- NA

trainList <- list(projected = list(polygons = poly, points = pts, img = lsat_t))

g <- function(x){as.character(st_geometry_type(x,F))}


## Maximum likelihood custom model
test_that("maximum likelihood model",
    expect_is(superClass(lsat_t, trainData = poly, responseCol = "class", model = "mlc", tuneLength = 1, trainPartition = 0.7, predict = FALSE), "superClass")
) 

for(type in c("polygons", "points")){
    if (!identical(Sys.getenv("NOT_CRAN"), "true") ) {
        if( type == "points") break
    }
    
    info <- paste(c("train type = ", type, " | coordinates = ", proj), collapse = "")       
    train <- trainList[[1]][[type]]
    img   <- trainList[[1]][["img"]]
    geometry <- if(type=="polygons") "POLYGON" else "POINT"
    ## No prediction    
    set.seed(1)
    sc <- superClass(img, trainData = train, nSamples = 50, nSamplesV = 50, responseCol = "class", model = "pls", 
            tuneGrid = data.frame(ncomp = 3), tuneLength = 1, trainPartition = 0.7, predict = FALSE)
    
    ## with prediction
    set.seed(1)    
    sc2 <- superClass(img, trainData = train, nSamples = 50, nSamplesV = 50, responseCol = "class", model = "pls", 
            tuneGrid = data.frame(ncomp = 3), tuneLength = 1, trainPartition = 0.7, predict = TRUE)
    
    ## Polygonbased CV
    set.seed(1)
    test_that("polygonbased CV", {
                expect_is(superClass(img, trainData = train, nSamples = 50,
                                responseCol = "class", model = "pls", 
                                tuneLength = 1, polygonBasedCV = TRUE, 
                                trainPartition = 0.5, predict = FALSE), 
                        c("RStoolbox", "superClass"), info = info)
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
                expect_equal(sum(values(sc2$map - sc3$map)), 0, info = info)
            })
    
    test_that("validation sample coords and geometries are returned", {                        
                expect_equal(g(sc$validation$validationGeometry), geometry, info = info) 
                expect_equal(g(sc2$validation$validationGeometry), geometry, info = info) 
                expect_equal(g(sc3$validation$validationGeometry), geometry, info = info) 
                expect_is(sc$validation$validationSamples, "sf", info = info) 
                expect_is(sc2$validation$validationSamples, "sf", info = info) 
                expect_is(sc3$validation$validationSamples, "sf", info = info) 
                expect_equal(colnames(sc$validation$validationSamples), c("reference", "prediction", "cell", "geometry"), info = info) 
                expect_equal(colnames(sc2$validation$validationSamples), c("reference", "prediction", "cell", "geometry"), info = info) 
                expect_equal(colnames(sc3$validation$validationSamples), c("reference", "prediction", "cell", "geometry"), info = info) 
            })
    
    test_that("predict.superClass map is identical to superClass$map",{
                expect_equal(sum(values(sc2$map) - values(predict(sc2, img))), 0)
            })
    
    test_that("prediction of probability layers", {   
                expect_is(sc4  <- superClass(img, trainData = train, nSamples = 50, ntree = 100, responseCol = "class", model = "rf",
                                predType = "prob", tuneLength = 1, predict = TRUE),  "superClass", info = info)
                expect_is(scp <- predict(sc4, img, predType = "prob"), "SpatRaster")
                expect_identical(dim(sc4$map)[3], 4, info=info)
                expect_equal(sum(values(sc4$map) - values(scp)), 0)
                expect_equal(as.numeric(unique(values(round(sum(sc4$map))))), 1)
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
    
    st_crs(train) <- NA
    nimg <- img
    crs(nimg) <- NA
    test_that("missing projection info", {
                expect_warning(superClass(nimg, trainData = train, minDist = 1, nSamples = 50, responseCol = "class", model = "pls", 
                                tuneGrid = data.frame(ncomp = 3), tuneLength = 1, trainPartition = 0.7, predict = FALSE), "missing projection")
            })
}

#
test_that("sp inputs",{
            expect_is( superClass(lsat_t, trainData = as_Spatial(poly), nSamples = 50, responseCol = "class", model = "pls",
                                  tuneGrid = data.frame(ncomp = 3), tuneLength = 1, trainPartition = 0.7, predict = FALSE), "superClass")
            
        })
#
test_that("terra inputs",{
            expect_is( superClass(lsat_t, trainData = poly, nSamples = 50, responseCol = "class", model = "pls",
                                  tuneGrid = data.frame(ncomp = 3), tuneLength = 1, trainPartition = 0.7, predict = FALSE), "superClass")
            
        })
#
if (identical(Sys.getenv("NOT_CRAN"), "true") ) {
    test_that("NA in raster remains NA",{
        expect_is(sc <- superClass(lsNA, trainData = pts, responseCol = "class", model = "rf",
                         filename = .terraTmpFile(), trainPartition = 0.7, predict = TRUE), "superClass")
        expect_equal(sum(is.na(sc$map[1:100,])), 100*ncol(lsNA))
        expect_false(anyNA(sc$map[101:nrow(lsNA),]))
    })
    
    ## Checks after clipping 
    test_that("fails if no validation points remain after clipping",{
                expect_error(sc <- superClass(lsNA, trainData = pts, minDist=1000, responseCol = "class", trainPartition = 0.7), "no validation points remained")
                expect_error(sc <- superClass(lsNA, trainData = poly, minDist=1000, responseCol = "class", trainPartition = 0.7), "no validation points remained")
            })
    
    ## Projection checks
    poly <- st_transform(poly, "epsg:4326")
    test_that("projection mismatch errors", 
            expect_error(superClass(lsat_t, trainData = poly, responseCol = "class"), "must have the same projection")
    )
}

test_that("sampling option", {
  train <- readRDS(system.file("external/trainingPoints_rlogo.rds", package="RStoolbox"))
  
  train_rose <- readRDS(system.file("external/trainingPolygons_lsat.rds", package="RStoolbox"))
  train_rose <- train_rose[train_rose$class == "forest" | train_rose$class == "water", ]
  train_rose$class <- factor(as.character(train_rose$class))
  
  for (sm in c("up", "down")) {
    expect_is(sc <- superClass(rlogo, trainData = train, responseCol = "class",
                         model = "rf", tuneLength = 1, trainPartition = 0.7, sampling = sm), "superClass")
    expect(!any(is.na(values(sc$map))), "Found na value on valid superClass while testing sampling methods")
  }
  
  expect_warning(superClass(rlogo, trainData = train, responseCol = "class",
                               model = "rf", tuneLength = 1, trainPartition = 0.7, sampling = "smote"), "Not enough observations.*to perform SMOTE")
  
  expect_is(sc <- superClass(lsat, trainData = train_rose, responseCol = "class",
                             model = "rf", tuneLength = 1, trainPartition = 0.7, sampling = "rose"), "superClass")
  expect(!any(is.na(values(sc$map))), "Found na value on valid superClass while testing rose-sampling")
  
})
